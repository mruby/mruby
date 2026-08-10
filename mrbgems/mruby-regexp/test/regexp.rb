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

assert("Regexp - character class range across the ASCII boundary") do
  # A range from an ASCII bound to a non-ASCII one used to be stored whole in
  # the codepoint list, which the matcher never reads below 128, so the ASCII
  # half of the range matched nothing.
  assert_equal "a", "a".match(/[a-Ā]/)[0]
  assert_equal "z", "z".match(/[a-Ā]/)[0]
  assert_equal "{", "{".match(/[a-Ā]/)[0]      # 0x7b, inside a-Ā
  assert_nil "A".match(/[a-Ā]/)                # 0x41, below the range
  assert_nil "`".match(/[a-Ā]/)                # 0x60, just below 'a'
  assert_equal "abĀz", "!abĀz!".match(/[a-Ā]+/)[0]
  # The non-ASCII half still answers on its own.
  assert_equal "Ā", "Ā".match(/[a-Ā]/)[0]
  assert_equal "À", "À".match(/[a-Ā]/)[0]
  assert_nil "ā".match(/[a-Ā]/)                # one past the upper bound
  # Negation reads the same class, so it rejected the ASCII half it had to
  # accept and accepted the half it had to reject.
  assert_nil "a".match(/[^a-Ā]/)
  assert_nil "Ā".match(/[^a-Ā]/)
  assert_equal "A", "A".match(/[^a-Ā]/)[0]
  assert_equal "ā", "ā".match(/[^a-Ā]/)[0]
  # The /i fold walks the bitmap, so it reaches the ASCII half once that half
  # is stored there. Non-ASCII case folding is still not applied.
  assert_equal "A", "A".match(/[a-Ā]/i)[0]
  assert_nil "A".match(/[^a-Ā]/i)
  # Ranges that stay on one side of the boundary are unaffected.
  assert_equal "b", "b".match(/[a-c]/)[0]
  assert_equal "ą", "ą".match(/[Ā-Đ]/)[0]
  assert_nil "a".match(/[Ā-Đ]/)
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
  # /i makes the two letter-case classes equivalent.
  assert_equal "abAB", "abAB".match(/[[:upper:]]+/i)[0]
  assert_equal "abAB", "abAB".match(/[[:lower:]]+/i)[0]
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

assert("Regexp - case insensitive character class") do
  # /i used to be folded in only where a single literal was emitted, so a
  # character class ignored it entirely.
  assert_true(/[abc]/i.match?("A"))
  assert_true(/[a-c]/i.match?("A"))
  assert_true(/[A-C]/i.match?("a"))
  assert_true(/[a-c]+/i.match?("AB"))
  assert_true Regexp.new("[a-c]", Regexp::IGNORECASE).match?("A")
  # A negated class matched what it had to reject, which is a false positive.
  assert_false(/[^a-c]/i.match?("A"))
  assert_false(/[^A-C]/i.match?("a"))
  assert_true(/[^a-c]/i.match?("d"))
  # Folding must not widen the class beyond the ASCII letters.
  assert_false(/[a-c]/i.match?("D"))
  assert_false(/[\[]/i.match?("{"))  # `[` and `{` are 32 apart but are not a case pair
  assert_false(/[@]/i.match?("`"))
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

  # A character class reads the inline-scoped flag, not the pattern-wide one.
  assert_equal 0, (/(?i)[a-c]/ =~ "A")
  assert_equal 0, (/(?i:[a-c])/ =~ "A")
  assert_nil (/(?i:[a-c])[a-c]/ =~ "AB")  # option must not leak past the `)`

  # The toggle inside a group is confined to that group.
  assert_equal 0, (/(a(?i)b)c/ =~ "aBc")
  assert_nil (/(a(?i)b)c/ =~ "aBC")       # trailing `c` is case-sensitive again

  # A backreference takes the options in effect where it appears, not the
  # pattern's own, so an inline toggle reaches it like any other atom.
  assert_equal 0, (/(a)(?i)\1/ =~ "aA")
  assert_equal 0, (/(a)(?i:\1)/ =~ "aA")
  assert_nil (/(?-i:(a)\1)/i =~ "aA")

  # m enables dot-matches-newline for its scope.
  assert_equal 0, (/(?m:a.b)/ =~ "a\nb")
  assert_nil (/a.b/ =~ "a\nb")

  # x (extended) cannot be scoped inline with the current architecture, so
  # turning it on is rejected.
  assert_raise(RegexpError) { Regexp.new("(?x)a b") }
  assert_raise(RegexpError) { Regexp.new("(?x:a b)") }

  # Turning it off is accepted, because Regexp#to_s writes a '-x' for every
  # pattern that is not extended and that form has to recompile.
  assert_equal 0, (/(?-x:a b)/ =~ "a b")
  assert_equal 0, (/(?i-mx:a)b/ =~ "Ab")
  assert_true Regexp.new("(?-mix:a b)").match?("a b")

  # The '-x' is dropped rather than honoured, so in a pattern that is
  # itself extended the whitespace stays stripped. CRuby matches "a b"
  # here.
  assert_true Regexp.new("(?-x:a b)", Regexp::EXTENDED).match?("ab")
end

assert("Regexp - comment groups (?#...)") do
  # The group is removed before the pattern is parsed, so it can stand
  # anywhere, including where an atom cannot.
  assert_true(/a(?#note)b/.match?("ab"))
  assert_true Regexp.new("(?#lead)ab").match?("ab")
  assert_true Regexp.new("ab(?#trail)").match?("ab")
  assert_true Regexp.new("a(?#)b").match?("ab")          # empty comment
  assert_true Regexp.new("a(?#no\nte)b").match?("ab")    # newline is comment text
  assert_equal ["ab", "ab"], Regexp.new("(a(?#c)b)").match("ab").to_a

  # The group is not an atom: a quantifier after it repeats what came before.
  assert_equal 0, (Regexp.new("a(?#x)*") =~ "aaa")
  assert_raise(RegexpError) { Regexp.new("(?#x)*") }

  # A backslash escapes the following byte, so \) does not close the group.
  assert_true Regexp.new("a(?#x\\)y)b").match?("ab")
  # ... but an escaped backslash does not reach the ')', which then closes
  # the group and leaves the second one unmatched.
  assert_raise(RegexpError) { Regexp.new("a(?#x\\\\)y)b") }

  # Comment groups do not nest: the first ')' closes, the second is unmatched.
  assert_raise(RegexpError) { Regexp.new("x(?#a(?#b))y") }

  # An unterminated group raises rather than swallowing the rest.
  assert_raise_with_message(RegexpError, "unterminated comment group: /a(?#note/") do
    Regexp.new("a(?#note")
  end

  # Inside a character class the same bytes are ordinary members.
  assert_true Regexp.new("a[(?#c)]b").match?("a#b")
  assert_true Regexp.new("a[(?#c)]b").match?("a(b")

  # An escaped '(' does not open a comment group.
  assert_raise(RegexpError) { Regexp.new("a\\(?#note)b") }
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

assert("Regexp - quantifier on a multibyte literal") do
  # The bytes of a multibyte literal used to be separate atoms, so a
  # quantifier bound to the last one: /Ā+/ was \xC4(\x80)+ and stopped after
  # one Ā. The byte counts below are what tells the two apart.
  assert_equal 4, "ĀĀ".match(/Ā+/)[0].bytesize
  assert_equal 4, "ĀĀ".match(/Ā*/)[0].bytesize
  assert_equal 6, "ĀĀĀ".match(/Ā{2,3}/)[0].bytesize
  assert_true "ĀĀ".match?(/Ā{2}/)
  assert_false "Ā".match?(/Ā{2}/)
  # Three and four byte characters take the same path.
  assert_equal 6, "日日".match(/日+/)[0].bytesize
  assert_equal 8, "𝕏𝕏".match(/𝕏+/)[0].bytesize
  # A quantified literal after another atom, and a non-greedy one.
  assert_equal 5, "aĀĀ".match(/aĀ+/)[0].bytesize
  assert_equal 2, "ĀĀ".match(/Ā+?/)[0].bytesize
  # Scanning must not split a run into one match per character.
  assert_equal [4, 2], "ĀĀxĀ".scan(/Ā+/).map { |s| s.bytesize }
  # An optional multibyte literal that is absent still matches empty.
  assert_equal 0, "z".match(/Ā?/)[0].bytesize
end

assert("Regexp - quantifier on an invalid multibyte literal") do
  # A byte above 127 is one atom only while it starts a whole character. The
  # sequences below never complete one, so each byte stands alone and the
  # quantifier binds to the byte in front of it, not to the pair.
  lead2 = "\xC4"  # starts a two byte character
  lead3 = "\xE3"  # starts a three byte character
  cont = "\x81"   # continuation byte

  # "x" is not a continuation byte, so `+` repeats "x".
  assert_equal 4, (lead2 + "xxx").match(Regexp.new(lead2 + "x+"))[0].bytesize
  assert_equal 4, (lead3 + "abb").match(Regexp.new(lead3 + "ab+"))[0].bytesize
  # The quantifier itself must not be taken for a continuation byte either.
  assert_equal 2, (lead2 + lead2).match(Regexp.new(lead2 + "+"))[0].bytesize
  # A sequence cut short by the end of the pattern emits its bytes one by one.
  assert_equal 2, (lead3 + cont).match(Regexp.new(lead3 + cont))[0].bytesize
  assert_equal 3, (lead3 + cont + cont).match(Regexp.new(lead3 + cont + "+"))[0].bytesize
  # A valid character right after an invalid lead byte is still one atom.
  assert_equal 5, (lead2 + "ĀĀ").match(Regexp.new(lead2 + "Ā+"))[0].bytesize
  # The subject side reads the same way: `.` takes the lead byte alone.
  assert_equal 1, (lead2 + "x").match(/./)[0].bytesize
  assert_equal 2, "Ā".match(/./)[0].bytesize
end

assert("Regexp - a byte that belongs to no character is a match position") do
  # A byte in 0x80-0xBF is the interior of a character only while a lead byte
  # in front of it reaches that far. One that stands on its own is a boundary
  # like any other, and the engines used to disagree about it: the literal
  # fast path matched there, the NFA never started a match there.
  b = "\x81"
  assert_equal 0, (b + b).match(Regexp.new(b + b)).begin(0)
  assert_equal 2, (b + b).match(Regexp.new(b + "+"))[0].bytesize
  assert_equal 2, (b + b).match(Regexp.new(b + "*"))[0].bytesize
  assert_equal 1, (b + b).match(Regexp.new(b + "?"))[0].bytesize
  assert_equal 1, ("x" + b + b).match(Regexp.new(b + "+")).begin(0)
  # Inside a character there is still no match position.
  assert_nil "あ".match(Regexp.new("\x81"))
  assert_nil "あ".match(Regexp.new("\x82"))
  assert_nil "\u{1D54F}".match(Regexp.new("\x95"))
  # Next to one there is.
  assert_equal 0, (b + "あ").match(Regexp.new(b)).begin(0)
  # Through pre_match, since #begin counts characters where the build has
  # them and bytes where it does not.
  assert_equal 3, ("あ" + b).match(Regexp.new(b)).pre_match.bytesize
end

assert("Regexp - an attempt in flight opens no match position inside a character") do
  # "ĵ" is C4 B5 and "µ" is C2 B5, so the two share their trailing byte. That
  # byte is the interior of "ĵ" and no match may start there, but the test
  # for it only ran while nothing was in flight. The branch of `.?` that
  # consumes the character parks a thread past it, and the attempt seeded at
  # the shared byte then matched it on its own, cutting "ĵ" in half.
  assert_nil "ĵ".match(/.?[µ]/)
  assert_nil "ĵ".gsub(/.?[µ]/, "!").match(/!/)
  assert_nil ("あ" + "ĵ").match(/.?[µ]/)
  # A character the class does hold is still found through the same branch.
  assert_equal 4, ("ĵ" + "µ").match(/.?[µ]/)[0].bytesize
  assert_equal 5, ("あ" + "µ").match(/.?[µ]/)[0].bytesize
  # And so is the byte itself where no lead byte reaches it.
  assert_equal 2, ("x" + "\xb5").match(/.?[µ]/)[0].bytesize
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

  # a bracket the pattern truncates leaves the scan with nothing after the
  # name, and the class is still the parser's error to report
  assert_raise_with_message(RegexpError, "unterminated character class: /[[:alpha/") do
    Regexp.new("[[:alpha", Regexp::EXTENDED)
  end
  assert_raise_with_message(RegexpError, "unterminated character class: /[[:alpha:/") do
    Regexp.new("[[:alpha:", Regexp::EXTENDED)
  end

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

  # a comment group is removed ahead of the line-comment pass, so its ')'
  # survives the '#' inside it
  re = Regexp.new("a (?#note) b", Regexp::EXTENDED)
  assert_true re.match?("ab")

  re = Regexp.new("a (?#note) b # tail\nc", Regexp::EXTENDED)
  assert_true re.match?("abc")

  assert_raise_with_message(RegexpError, "unterminated comment group: /a (?#note/") do
    Regexp.new("a (?#note", Regexp::EXTENDED)
  end

  # inspect shows x flag
  assert_equal "/abc/x", Regexp.new("abc", Regexp::EXTENDED).inspect

  # to_s shows x flag
  assert_equal "(?x-mi:abc)", Regexp.new("abc", Regexp::EXTENDED).to_s

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

assert("Regexp - a named group makes plain groups non-capturing") do
  # Onigmo's ONIG_OPTION_DONT_CAPTURE_GROUP, which CRuby turns on once the
  # pattern declares a named group: (...) then groups without capturing.
  md = /(?<a>a)(b)/.match("ab")
  assert_equal 2, md.size
  assert_equal ["ab", "a"], md.to_a
  assert_equal ["a"], md.captures
  assert_nil md[2]
  assert_raise_with_message(IndexError, "index 2 out of matches") { md.begin(2) }
  assert_equal "a", md[:a]

  # a plain group written before the named group is demoted just the same,
  # which is what the pre-scan buys: the parser reaches it before it has seen
  # the declaration that decides the question
  md = /(a)(?<b>b)/.match("ab")
  assert_equal 2, md.size
  assert_equal ["ab", "b"], md.to_a
  assert_equal ["b"], md.captures
  assert_equal "b", md[1]
  assert_equal "b", md[:b]

  # the shrunken count is what $2, $+ and a \2 in a replacement read
  "ab" =~ /(?<a>a)(b)/
  assert_nil $2
  assert_equal "a", $+
  assert_equal "[]", "ab".sub(/(?<a>a)(b)/, '[\2]')

  # (?<= and (?<! open a lookbehind, not a named group, so they demote nothing
  assert_equal ["b", "b"], /(?<=a)(b)/.match("ab").to_a
  assert_equal ["b", "b"], /(?<!x)(b)/.match("ab").to_a
  # nor does a "(?<" that is escaped or sits inside a character class
  assert_equal ["(<a>b", "b"], /\(?<a>(b)/.match("(<a>b").to_a
  assert_equal ["(?<b", "b"], /[(?<a>]+(b)/.match("(?<b").to_a
  assert_equal ["a(?<b", "b"], /[[:alpha:](?<]+(b)/.match("a(?<b").to_a
  # nor one inside a (?#...) comment group, which is gone before the scan runs
  assert_equal ["b", "b"], /(?# (?<a>x )(b)/.match("b").to_a

  # in /x mode the scan reads the pattern after free-spacing and comments go
  assert_equal ["xy", "x"], /(?<a>x) # (b)
                             (y)/x.match("xy").to_a
  assert_equal ["y", "y"], /# (?<a>x)
                            (y)/x.match("y").to_a

  # a truncated "(?<" is still the parser's error, not a silent named group
  assert_raise(RegexpError) { Regexp.new("(?<") }

  # the scan runs on every pattern, so a truncated POSIX bracket reaches
  # skip_posix_bracket() without /x too, and is still the parser's error
  assert_raise_with_message(RegexpError, "unterminated character class: /[[:alpha/") do
    Regexp.new("[[:alpha")
  end
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

assert("Regexp - backreference under /i") do
  # The comparison against the captured text has to fold case too, otherwise
  # `\1` stays case-sensitive while the rest of the pattern does not.
  assert_equal "aA", /(a)\1/i.match("aA")[0]
  assert_equal "Hello hELLO", /(\w+) \1/i.match("Hello hELLO world")[0]
  assert_nil /(a)\1/i.match("ab")
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
  # /i folds the comparison against the captured text
  assert_equal "aA", "aA".match(/(?<n>a)\k<n>/i)[0]
  assert_nil "ab".match(/(?<n>a)\k<n>/i)
  # an unknown name is an error
  assert_raise(RegexpError) { Regexp.new("\\k<missing>") }

  # once the pattern has a named group a numbered backreference is rejected,
  # whatever its spelling, because there is no longer a number to reach
  msg = "numbered backref/call is not allowed. (use name)"
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(?<b>b)\\1/") do
    Regexp.new("(a)(?<b>b)\\1")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(?<b>b)\\k<1>/") do
    Regexp.new("(a)(?<b>b)\\k<1>")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(?<b>b)\\k<-1>/") do
    Regexp.new("(a)(?<b>b)\\k<-1>")
  end
  assert_raise(RegexpError) { Regexp.new("(?<b>b)\\k'1'") }
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

assert("Regexp - lookbehind rejects a class that can match a multibyte character") do
  # A class holding non-ASCII members consumes one byte here and two there,
  # so no single rewind width is right. Refusing the pattern beats rewinding
  # into the middle of a character, where a positive lookbehind reports no
  # match and a negative one reports a match.
  assert_raise(RegexpError) { Regexp.new("(?<=[Ā])x") }
  assert_raise(RegexpError) { Regexp.new("(?<![Ā])b") }
  assert_raise(RegexpError) { Regexp.new("(?<=[Ā-ă])x") }
  assert_raise(RegexpError) { Regexp.new("(?<=[aĀ])x") }
  assert_raise(RegexpError) { Regexp.new("(?<=[Ā]{2})x") }
  # a negated class always admits non-ASCII, whatever its members are
  assert_raise(RegexpError) { Regexp.new("(?<=[^あ])x") }
  assert_raise(RegexpError) { Regexp.new("(?<![^a])b") }
  # the uppercase shorthands carry the same catch-all
  assert_raise(RegexpError) { Regexp.new("(?<=a\\W)x") }
  assert_raise(RegexpError) { Regexp.new("(?<=\\W\\W)x") }
  assert_raise(RegexpError) { Regexp.new("(?<=\\D)x") }
  assert_raise(RegexpError) { Regexp.new("(?<=\\S)x") }
end

assert("Regexp - lookbehind measures an ASCII-only class") do
  assert_equal "x", "ax".match(/(?<=[a-z])x/)[0]
  assert_nil "1x".match(/(?<=[a-z])x/)
  assert_equal "x", "1x".match(/(?<=\d)x/)[0]
  assert_equal "x", " x".match(/(?<=\s)x/)[0]
  # a multibyte literal compiles to a run of one-byte instructions, so it
  # keeps its exact width and must keep measuring
  assert_equal "x", "Āx".match(/(?<=Ā)x/)[0]
  assert_nil "bx".match(/(?<=Ā)x/)
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

assert("Regexp - pattern too large for its jump targets is refused") do
  # Jump targets live in a 16-bit field, so a program that outgrows the field
  # used to wrap them and jump to an unrelated instruction: the pattern then
  # quietly stopped matching text it describes instead of reporting anything.
  # Each (?:abc) unit costs three instructions and the bound is on the whole
  # program, so the two counts below sit either side of it.
  assert_kind_of Regexp, Regexp.new("(?:abc){21844}")
  assert_raise_with_message(RegexpError, "regexp too large: /(?:abc){21845}/") do
    Regexp.new("(?:abc){21845}")
  end

  # the shapes that used to answer wrongly rather than raise: a quantifier
  # whose skip target is patched past the bound, and an alternation whose
  # branch and exit targets both wrap
  assert_raise(RegexpError) { Regexp.new("(?:abc){21844}x*y") }
  assert_raise(RegexpError) { Regexp.new("(?:abc){30000}(?:y|z)") }

  # a quantifier the parser still accepts reaches the bound on its own once
  # the repeated atom costs more than one instruction
  assert_raise(RegexpError) { Regexp.new("(?:ab){32768}") }
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
