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

assert("Regexp - a repetition stops on an empty iteration") do
  # A repetition whose body matches empty runs that iteration and then stops,
  # so a body that prefers the empty branch ends the loop at once instead of
  # going around again on the branch that consumes.
  assert_equal "", "a".match(/(|a)*/)[0]
  assert_equal "", "aaa".match(/(|a)*/)[0]
  assert_equal "", "a".match(/(?:|a)+/)[0]
  # a body that can only match empty after consuming still consumes first
  assert_equal "aaa", "aaa".match(/(a|)*/)[0]
  assert_equal "aa", "aab".match(/(a?)*/)[0]
end

assert("Regexp - a repetition keeps its last, empty iteration's capture") do
  # The final iteration is the empty one, and the group keeps what it
  # captured: the empty string where the loop stopped. The linear-time engine
  # used to drop that iteration and report the previous one's text, or nil
  # when there was no previous one.
  md = "a".match(/(a?)*/)
  assert_equal "a", md[0]
  assert_equal "", md[1]
  assert_equal 1, md.begin(1)
  assert_equal "", "aab".match(/(a*)*b/)[1]
  assert_equal "", "a".match(/(a|)*/)[1]
  assert_equal "", "a".match(/(a?)+/)[1]
  # with no earlier iteration the group still participates
  assert_equal "", "b".match(/(a?)*/)[1]
  assert_equal "", "".match(/(a?)*/)[1]
  assert_equal "", "b".match(/(a*)*b/)[1]
  # a nullable body nested in a repetition reaches the same answer
  assert_equal "", "a".match(/((a?)*)*/)[1]
  # both engines agree: a lookaround routes the same pattern to the other one
  assert_equal "", "a".match(/(?=a)(a?)*/)[1]
  assert_equal 1, "a".match(/(?=a)(a?)*/).begin(1)
  assert_equal "", "b".match(/(?=b)(a?)*/)[1]
end

assert("Regexp - a repetition whose body always consumes is unaffected") do
  assert_equal "b", "ab".match(/(a|b)*/)[1]
  assert_nil "c".match(/(a|b)*/)[1]
  assert_equal "aa", "aa".match(/(a)*/)[0]
  assert_equal "a", "aa".match(/(a)*/)[1]
  assert_equal ["", "b", ""], "ab".split(/(?:a?)*/, -1)
end

assert("String#split and String#scan see the empty iteration's capture") do
  assert_equal ["", "", "b", "", ""], "ab".split(/(a?)*/, -1)
  assert_equal [[""], [""], [""]], "ab".scan(/(a?)*/)
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

assert("Regexp - \\Z matches before a trailing newline under both engines") do
  # \Z is the string end or the position just before a final newline. The
  # lazy quantifier routes the pattern to the backtracking engine, which had
  # no case for the opcode and so failed every \Z it saw.
  assert_equal 0, "a" =~ /a\Z/
  assert_equal 0, "a\n" =~ /a\Z/
  assert_nil "a\n\n" =~ /a\Z/
  assert_nil "ab" =~ /a\Z/
  assert_equal 0, "a" =~ /a\Za*?/
  assert_equal 0, "a\n" =~ /a\Z.*?/
  assert_nil "a\n\n" =~ /a\Z.*?/
  assert_nil "ab" =~ /a\Zb*?/
  assert_equal "aX\n", "ab\n".sub(/b\Z(?=)/, "X")
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

assert("Regexp - /i folds an ASCII letter's class whole") do
  # U+017F folds to "s" and U+212A to "k". They are the only two foldings
  # whose result is an ASCII letter, and every build carries them, so that
  # folding "ASCII only" covers the whole of the equivalence class an ASCII
  # letter belongs to rather than the part of it that is ASCII. Left out, the
  # negated forms below would accept what they were written to reject.
  # Both sources lie above ASCII, so they are characters to fold only where
  # the pattern and the subject are read as characters.
  skip unless __ENCODING__ == "UTF-8"
  kelvin = "K"
  long_s = "ſ"
  assert_true Regexp.new("k", Regexp::IGNORECASE).match?(kelvin)
  assert_true Regexp.new("K", Regexp::IGNORECASE).match?(kelvin)
  assert_true Regexp.new("[k]", Regexp::IGNORECASE).match?(kelvin)
  assert_true Regexp.new("[a-z]", Regexp::IGNORECASE).match?(kelvin)
  assert_true Regexp.new("[j-l]", Regexp::IGNORECASE).match?(kelvin)
  assert_false Regexp.new("[^k]", Regexp::IGNORECASE).match?(kelvin)
  assert_true Regexp.new(kelvin, Regexp::IGNORECASE).match?("k")
  assert_true Regexp.new(kelvin, Regexp::IGNORECASE).match?("K")
  assert_true Regexp.new("[#{kelvin}]", Regexp::IGNORECASE).match?("K")
  assert_true Regexp.new("s", Regexp::IGNORECASE).match?(long_s)
  assert_false Regexp.new("[^s]", Regexp::IGNORECASE).match?(long_s)
  assert_true Regexp.new(long_s, Regexp::IGNORECASE).match?("S")
  # A backreference compares the same way, so the capture and the repeat need
  # not hold the same bytes.
  assert_equal "k#{kelvin}", "k#{kelvin}".match(Regexp.new("(k)\\1", Regexp::IGNORECASE))[0]
  # Without /i none of it folds.
  assert_false Regexp.new("k").match?(kelvin)
  assert_true Regexp.new("[^k]").match?(kelvin)
end

assert("Regexp - /i keeps the word class inside ASCII") do
  # `\w` is [a-zA-Z0-9_] and no more, and [:word:] and [:ascii:] are sets
  # ASCII defines the same way, so /i folds none of them across the boundary:
  # the fold of a member that leaves ASCII leaves the set. CRuby reads them
  # the same way. The negated forms are where it shows: [^\w] under /i has to
  # accept U+212A and U+017F, which are not word characters, and used to
  # reject them because the closure of [k] and [s] had been applied to `\w`.
  # Both sources lie above ASCII, so they are characters only where the
  # pattern and the subject are read as characters.
  skip unless __ENCODING__ == "UTF-8"
  kelvin = "K"
  long_s = "ſ"
  [kelvin, long_s].each do |ch|
    assert_false Regexp.new("[\\w]", Regexp::IGNORECASE).match?(ch)
    assert_true Regexp.new("[^\\w]", Regexp::IGNORECASE).match?(ch)
    assert_false Regexp.new("[[:ascii:]]", Regexp::IGNORECASE).match?(ch)
    assert_true Regexp.new("[^[:ascii:]]", Regexp::IGNORECASE).match?(ch)
    # `\W` holds neither letter and everything above ASCII, so it takes both
    # with or without the fold; the negated form is what a fold would break.
    assert_true Regexp.new("[\\W]", Regexp::IGNORECASE).match?(ch)
    assert_false Regexp.new("[^\\W]", Regexp::IGNORECASE).match?(ch)
    # Outside a class the shorthand never folded, and still does not.
    assert_false Regexp.new("\\w", Regexp::IGNORECASE).match?(ch)
    assert_true Regexp.new("\\W", Regexp::IGNORECASE).match?(ch)
    # /i does not move either in or out of [:word:], whatever the set holds
    # (this gem's is the ASCII word characters; CRuby's holds every Unicode
    # word character, these two among them).
    assert_equal Regexp.new("[[:word:]]").match?(ch),
                 Regexp.new("[[:word:]]", Regexp::IGNORECASE).match?(ch)
    assert_equal Regexp.new("[^[:word:]]").match?(ch),
                 Regexp.new("[^[:word:]]", Regexp::IGNORECASE).match?(ch)
  end
  # A letter written out beside the shorthand folds as it does on its own:
  # the class then holds it by name as well as through `\w`, and the name is
  # what folds. Either case of the letter, in either order, and a range too.
  assert_true Regexp.new("[\\ws]", Regexp::IGNORECASE).match?(long_s)
  assert_true Regexp.new("[\\wS]", Regexp::IGNORECASE).match?(long_s)
  assert_true Regexp.new("[k\\w]", Regexp::IGNORECASE).match?(kelvin)
  assert_true Regexp.new("[\\wa-z]", Regexp::IGNORECASE).match?(long_s)
  assert_false Regexp.new("[^\\ws]", Regexp::IGNORECASE).match?(long_s)
  # Naming one letter folds that letter and no other.
  assert_false Regexp.new("[\\wk]", Regexp::IGNORECASE).match?(long_s)
  assert_false Regexp.new("[\\ws]", Regexp::IGNORECASE).match?(kelvin)
  # The other direction is untouched: a member above ASCII still folds to the
  # letter, and reaches the letter's other case through it.
  assert_true Regexp.new("[\\w#{long_s}]", Regexp::IGNORECASE).match?("S")
  assert_false Regexp.new("[^\\w#{long_s}]", Regexp::IGNORECASE).match?("S")
  # The other POSIX brackets fold like a written range: [:lower:] holds `k`,
  # so under /i it reaches U+212A, and [^[:alpha:]] rejects U+017F.
  assert_true Regexp.new("[[:lower:]]", Regexp::IGNORECASE).match?(kelvin)
  assert_true Regexp.new("[[:alpha:]]", Regexp::IGNORECASE).match?(long_s)
  assert_false Regexp.new("[^[:alpha:]]", Regexp::IGNORECASE).match?(long_s)
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

  # x (extended) is scoped inline like the other two: the toggle form
  # reaches the end of the enclosing group, the scoped form its own body.
  assert_equal 0, (/(?x)a b/ =~ "ab")
  assert_nil (/(?x)a b/ =~ "a b")
  assert_equal 0, (/(?x:a b)c d/ =~ "abc d")
  assert_equal 0, (/(a(?x)b c)d e/ =~ "abcd e")
  assert_equal 0, (/(?<n>(?x)a b)c d/ =~ "abc d")
  assert_equal 0, (/(?=(?x)a b)ab c/ =~ "ab c")
  assert_equal 0, (/(?xi)a b/ =~ "AB")
  assert_equal 0, (/(?x)a b(?-x)c d/ =~ "abc d")
  assert_equal 0, (/(?x:a(?-x:b c)d)/ =~ "ab cd")

  # Free-spacing follows the scope: a comment runs to the end of the line,
  # a (?# group is dropped as always, and an escape or a class keeps its
  # whitespace.
  assert_equal 0, (/(?x)a#c
b/ =~ "ab")
  assert_equal 0, (/(?x)(?#c d) e/ =~ "e")
  assert_equal 0, (/(?x)a\ b/ =~ "a b")
  assert_equal 0, (/(?x)[a b]/ =~ " ")
  assert_equal 0, (/[(?x] a/ =~ "( a")
  assert_equal 0, (/\(?x a/ =~ "(x a")

  # A comment swallows the rest of its line, closing parenthesis included,
  # as it does in CRuby.
  assert_true Regexp.new("(?x)a #b)").match?("a")
  assert_raise(RegexpError) { Regexp.new("(?x)a #b\n(c") }

  # Turning it off inside a pattern that is itself extended brings the
  # whitespace back for that scope.
  assert_equal 0, (/(?-x:a b)/ =~ "a b")
  assert_equal 0, (/(?i-mx:a)b/ =~ "Ab")
  assert_true Regexp.new("(?-mix:a b)").match?("a b")
  assert_true Regexp.new("(?-x:a b)", Regexp::EXTENDED).match?("a b")
  assert_true Regexp.new("(?-x)a b", Regexp::EXTENDED).match?("a b")
  assert_true Regexp.new("(?x)a b", Regexp::EXTENDED).match?("ab")
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

assert("Regexp - atomic group (?>...)") do
  # The body's first match is its only one: what follows cannot make it
  # give text back or take another branch, where a plain group can.
  assert_equal 0, /(?>a)+b/ =~ "aab"
  assert_equal 0, /(?:a+)ab/ =~ "aaab"
  assert_nil /(?>a+)ab/ =~ "aaab"
  assert_equal 0, /(?>a+)b/ =~ "aaab"
  assert_equal 0, /(?:a|ab)c/ =~ "abc"
  assert_nil /(?>a|ab)c/ =~ "abc"
  assert_equal 0, /(?>ab|a)c/ =~ "abc"

  # A repeated atomic group still gives back whole iterations: only the
  # inside of each one is closed to backtracking.
  assert_equal 0, /(?>a)+a/ =~ "aa"
  assert_equal 0, /(?>ab)+c/ =~ "ababc"
  assert_nil /(?>ab)+c/ =~ "abbc"
  assert_equal 0, /(?>a){2}b/ =~ "aab"
  assert_equal 1, /(?>ab)*?b/ =~ "abab"
  assert_equal 0, /(?>ab)|x/ =~ "x"

  # Once a group is closed, a failure after it fails the group as a whole,
  # even an alternation at the top of its body.
  assert_nil /(?>a(?>b|bc)|abcd)d/ =~ "abcd"
  # Before it is closed, its body backtracks as any other does, past an
  # inner atomic group that already closed.
  assert_equal "xy", /(?>(x|xy)(?>a)b)/.match("xyab")[1]
  # Sequential atomic groups at the same depth cut independently.
  assert_nil /(?>x(?>a)(?>b)y)/.match("xabz")
  assert_equal 0, /(?>x(?>a)(?>b)y)/ =~ "xaby"

  # A repetition whose body can match empty runs until the engine's recursion
  # limit stops it. Reached inside the body, that stop is not a cut.
  assert_equal 0, /(?>(?:b*)+)/ =~ ""
  assert_equal 0, /(?>(?:a*)*)b/ =~ "aab"

  # Captures written inside the body stay when the group matches, and are
  # unset again when a cut fails the group.
  assert_equal "a", /(?>(a)+)b/.match("aab")[1]
  assert_nil /(?:(?>(a))x|a)b/.match("ab")[1]
  assert_equal "a", /(?>(a)|ab)b/.match("ab")[1]

  # Inside a lookaround, the cut stays inside it.
  assert_equal 0, /(?=(?>a+)b)a/ =~ "aab"
  assert_nil /(?=(?>a+)ab)a/ =~ "aab"
  assert_equal 0, /(?!(?>a)b)a/ =~ "aac"

  # Options toggled in the body end with it, as in any group.
  assert_equal 0, /(?>a(?i)x)b/ =~ "aXb"
  assert_nil /(?>a(?i)x)B/ =~ "aXb"

  # It reads back through to_s, and free-spacing applies to its body.
  assert_equal 0, Regexp.new(/(?>a)+b/.to_s) =~ "aab"
  assert_equal 0, Regexp.new("(?> a b )c", Regexp::EXTENDED) =~ "abc"

  assert_raise(RegexpError) { Regexp.new("(?>a") }
  assert_raise(RegexpError) { Regexp.new("(?>") }
  # not a fixed-length construct, so not allowed in a lookbehind
  assert_raise(RegexpError) { Regexp.new("(?<=(?>a))b") }
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

assert("Regexp - the /x pass and the named-group scan skip the same constructs") do
  # Two walks read the pattern before the parser does: the /x free-spacing
  # pass and the named-group pre-scan. Both have to step over the same
  # escapes, character classes and POSIX brackets, so each row below is read
  # by both at once. A rule lost from the free-spacing pass strips a space it
  # should have kept; the same rule lost from the pre-scan turns a bracketed
  # "(?<" into a phantom named group, which demotes the plain (b) that
  # follows and shortens the match. Either way the row fails.
  x = Regexp::EXTENDED

  # an escape pair hides the '(' from both
  assert_equal ["(<a>b", "b"],
               Regexp.new('\(?<a> (b)', x).match("(<a>b").to_a

  # a character class hides "(?<" and keeps its own spaces
  assert_equal ["(?< b", "b"],
               Regexp.new('[(?< a>]+ (b)', x).match("(?< b").to_a

  # a ']' written first is a member, so the class runs past it
  assert_equal ["] (?<b", "b"],
               Regexp.new('[] (?<a>]+(b)', x).match("] (?<b").to_a
  assert_equal ["zzb", "b"],
               Regexp.new('[^] (?<a>]+(b)', x).match("zzb").to_a

  # a POSIX bracket's ']' does not close the class either
  assert_equal ["a (?<b", "b"],
               Regexp.new('[[:alpha:] (?<a>]+(b)', x).match("a (?<b").to_a

  # a `\u{...}` list is one escape, so its separating space survives /x and
  # its bytes are not read as pattern syntax
  assert_equal ["ab"], Regexp.new('\u{61 62}', x).match("ab").to_a
  assert_equal ["abcd", "c", "d"],
               Regexp.new('\u{61 62}(c)(d)', x).match("abcd").to_a
  assert_equal ["abcd", "c"],
               Regexp.new('\u{61 62}(?<n>c)(d)', x).match("abcd").to_a
  assert_equal ["abcd", "c", "d"],
               Regexp.new('[\u{61 62}]+(c)(d)', x).match("abcd").to_a

  # With no whitespace, no #comment and no (?#...) to remove, /x rewrites
  # nothing, so both compiles must agree; they do not take the same road,
  # though: without /x the pre-scan reads the pattern as written, while with
  # /x it reads what the free-spacing pass emitted. The two walks disagreeing
  # about where a class or an escape ends is exactly what shows up here.
  [
    ['\(?<a>(b)',            "(<a>b"],
    ['[(?<a>]+(b)',          "(?<b"],
    ['[](?<a>]+(b)',         "](?<b"],
    ['[^](?<a>]+(b)',        "zzb"],
    ['[[:alpha:](?<a>]+(b)', "a(?<b"],
    ['[\]](?<a>x)(y)',       "]xy"],
    ['\\\\(?<a>x)(y)',       "\\xy"],
    ['\u{61}(?<n>b)(c)',     "abc"],
    ['[\u{61}]+(?<n>b)(c)',  "abc"],
    ['(a)(?<b>b)',           "ab"],
    ["\\(?'a'(b)",           "('a'b"],
    ["[(?'a]+(b)",           "(?'b"],
  ].each do |pat, subject|
    assert_equal Regexp.new(pat).match(subject).to_a,
                 Regexp.new(pat, x).match(subject).to_a
  end

  # A `\u{...}` list is not a place a named group can be declared, so the
  # scan must not read "(?<" out of one. The list here is malformed either
  # way and the pattern is rejected either way, but which error comes first
  # depends on the scan: taking the "(?<" for a declaration turns on the
  # demotion that rejects the leading \1 before the parser ever reaches the
  # bad list. CRuby reports the list, and so does the scan that treats
  # `\u{...}` as one escape.
  assert_raise_with_message(RegexpError,
                            "invalid Unicode list: /\\1\\u{(?<a>/") do
    Regexp.new("\\1\\u{(?<a>")
  end
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

assert("Regexp - a named group can be written (?'name'...)") do
  # A definition has two spellings, and \k already read both, so the parser
  # used to accept a reference to a name it refused to introduce.
  md = /(?'x'a)/.match("a")
  assert_equal ["a", "a"], md.to_a
  assert_equal "a", md[:x]
  assert_equal ["year", "month"], /(?'year'\d+)-(?'month'\d+)/.names
  assert_equal({"year" => [1], "month" => [2]},
               /(?'year'\d+)-(?'month'\d+)/.named_captures)

  # either spelling of \k reaches a group written in either spelling
  assert_equal "aa", "aa".match(/(?'n'\w)\k<n>/)[0]
  assert_equal "aa", "aa".match(/(?<n>\w)\k'n'/)[0]
  assert_equal "aa", "aa".match(/(?'n'\w)\k'n'/)[0]

  # the two spellings write into one registry: a name given twice is reported
  # once however each of them was spelled
  assert_equal ["t"], /(?<t>\w)(?'t'\w)/.names
  assert_equal ["xy", "x", "y"], /(?<a>x)(?'b'y)/.match("xy").to_a

  # a name runs to its own terminator, so the other spelling's terminator is
  # a member of it rather than the end
  assert_equal ["a>b"], /(?'a>b'x)/.names
  assert_equal ["a'b"], /(?<a'b>x)/.names

  # nesting, quantifiers and /i are the group's own business either way
  assert_equal ["ab", "ab", "b"], /(?'o'a(?'i'b))/.match("ab").to_a
  assert_equal ["abab", "ab"], /(?'a'ab)+/.match("abab").to_a
  assert_equal ["AB", "AB"], /(?'a'ab)/i.match("AB").to_a

  # a name is still required, and still has to be terminated
  assert_raise(RegexpError) { Regexp.new("(?''x)") }
  assert_raise(RegexpError) { Regexp.new("(?'x") }
  assert_raise(RegexpError) { Regexp.new("(?'") }
end

assert("Regexp - a (?'name'...) group demotes plain groups too") do
  # The pre-scan settles the demotion before the parser runs, so it has to
  # know both spellings: reading "(?<" alone left /(a)(?'b'b)/ numbering the
  # plain group that the declaration demotes.
  md = /(?'a'a)(b)/.match("ab")
  assert_equal 2, md.size
  assert_equal ["ab", "a"], md.to_a
  assert_nil md[2]

  md = /(a)(?'b'b)/.match("ab")
  assert_equal ["ab", "b"], md.to_a
  assert_equal "b", md[:b]

  assert_equal "[]", "ab".sub(/(?'a'a)(b)/, '[\2]')

  # and the numbers the declaration took away cannot be referred to
  msg = "numbered backref/call is not allowed. (use name)"
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(?'b'b)\\1/") do
    Regexp.new("(a)(?'b'b)\\1")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(?'b'b)\\k<1>/") do
    Regexp.new("(a)(?'b'b)\\k<1>")
  end

  # an escaped or bracketed "(?'" declares nothing, so the plain group that
  # follows keeps its number
  assert_equal ["('a'b", "b"], /\(?'a'(b)/.match("('a'b").to_a
  assert_equal ["(?'b", "b"], /[(?'a]+(b)/.match("(?'b").to_a
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
  msg = "too big number"
  assert_raise_with_message(RegexpError, "#{msg}: /(a)\\k<4294967297>/") do
    Regexp.new("(a)\\k<4294967297>")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)\\k<-4294967297>/") do
    Regexp.new("(a)\\k<-4294967297>")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(b)\\k<4294967298>/") do
    Regexp.new("(a)(b)\\k<4294967298>")
  end
end

assert("Regexp - \\k group reference errors say which failure it was") do
  # A \k reference fails in four ways and CRuby gives each its own message.
  # They used to collapse into one, so a pattern that misspelled a name and a
  # pattern that named a group it never opened read the same.

  # a name that is neither `-`? digits nor a name any group carries
  assert_raise_with_message(RegexpError, "invalid group name <1x>: /(a)\\k<1x>/") do
    Regexp.new("(a)\\k<1x>")
  end
  assert_raise_with_message(RegexpError, "invalid group name <-x>: /(a)\\k<-x>/") do
    Regexp.new("(a)\\k<-x>")
  end
  # `-` with no digits behind it
  assert_raise_with_message(RegexpError, "invalid group name <->: /(a)\\k<->/") do
    Regexp.new("(a)\\k<->")
  end
  # group 0 is the whole match, which \k cannot name in either spelling.
  # The message quotes the name in <> whichever delimiter wrote it.
  assert_raise_with_message(RegexpError, "invalid group name <0>: /(a)\\k<0>/") do
    Regexp.new("(a)\\k<0>")
  end
  assert_raise_with_message(RegexpError, "invalid group name <-0>: /(a)\\k<-0>/") do
    Regexp.new("(a)\\k<-0>")
  end
  assert_raise_with_message(RegexpError, "invalid group name <0>: /(a)\\k'0'/") do
    Regexp.new("(a)\\k'0'")
  end

  # the name is read whole before it is converted, so digits followed by
  # anything else is a malformed name and never an oversized number
  assert_raise_with_message(RegexpError,
                            "invalid group name <99999999999999999999x>: /(a)\\k<99999999999999999999x>/") do
    Regexp.new("(a)\\k<99999999999999999999x>")
  end

  # a number past the bound, either sign
  assert_raise_with_message(RegexpError, "too big number: /(a)\\k<2147483648>/") do
    Regexp.new("(a)\\k<2147483648>")
  end
  assert_raise_with_message(RegexpError, "too big number: /(a)\\k<-2147483648>/") do
    Regexp.new("(a)\\k<-2147483648>")
  end

  # a number within the bound that names no group: a different message from
  # the one above, and the bound is where they part
  msg = "invalid backref number/name"
  assert_raise_with_message(RegexpError, "#{msg}: /(a)\\k<2147483647>/") do
    Regexp.new("(a)\\k<2147483647>")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)\\k<5>/") do
    Regexp.new("(a)\\k<5>")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)\\k'5'/") do
    Regexp.new("(a)\\k'5'")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)\\k<-5>/") do
    Regexp.new("(a)\\k<-5>")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(b)\\k<-3>/") do
    Regexp.new("(a)(b)\\k<-3>")
  end

  # a name no group carries
  assert_raise_with_message(RegexpError,
                            "undefined name <_nope> reference: /(a)\\k<_nope>/") do
    Regexp.new("(a)\\k<_nope>")
  end
  assert_raise_with_message(RegexpError,
                            "undefined name <_nope> reference: /(a)\\k'_nope'/") do
    Regexp.new("(a)\\k'_nope'")
  end
  # only `-` leads a number, so `+1` is a name and fails as one
  assert_raise_with_message(RegexpError,
                            "undefined name <+1> reference: /(a)\\k<+1>/") do
    Regexp.new("(a)\\k<+1>")
  end

  # a named pattern refuses a numbered reference, but only once the name is
  # read as a number at all: a malformed one and an oversized one are still
  # reported for what they are
  assert_raise_with_message(RegexpError, "invalid group name <1x>: /(a)(?<b>b)\\k<1x>/") do
    Regexp.new("(a)(?<b>b)\\k<1x>")
  end
  assert_raise_with_message(RegexpError, "invalid group name <0>: /(a)(?<b>b)\\k<0>/") do
    Regexp.new("(a)(?<b>b)\\k<0>")
  end
  assert_raise_with_message(RegexpError,
                            "too big number: /(a)(?<b>b)\\k<99999999999999999999>/") do
    Regexp.new("(a)(?<b>b)\\k<99999999999999999999>")
  end
  assert_raise_with_message(RegexpError,
                            "numbered backref/call is not allowed. (use name): /(a)(?<b>b)\\k<5>/") do
    Regexp.new("(a)(?<b>b)\\k<5>")
  end

  # leading zeros are digits like any other, not a malformed name
  assert_equal "aa", "aa".match(Regexp.new("(a)\\k<01>"))[0]
  assert_equal "aa", "aa".match(Regexp.new("(a)\\k<-01>"))[0]

  # The name is a length-counted slice of the pattern, so a name holding a NUL
  # is quoted whole. CRuby builds these messages through a C string and stops
  # at the NUL, reporting `undefined name <a` for the first of the two.
  assert_raise_with_message(RegexpError,
                            "undefined name <a\0b> reference: /(a)\\k<a\0b>/") do
    Regexp.new("(a)\\k<a\0b>")
  end
  assert_raise_with_message(RegexpError,
                            "invalid group name <1\0>: /(a)\\k<1\0>/") do
    Regexp.new("(a)\\k<1\0>")
  end
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

assert("Regexp - lookbehind over a class that can match a multibyte character") do
  # A class consumes exactly one character whatever its members are, so the
  # rewind steps back that many characters rather than assuming a byte each.
  # A build that reads its strings by byte has one byte per character, so it
  # has nothing here to tell the two rewinds apart.
  skip unless __ENCODING__ == "UTF-8"
  assert_equal "x", "Āx".match(/(?<=[Ā])x/)[0]
  assert_nil "ax".match(/(?<=[Ā])x/)
  assert_equal "x", "Āx".match(/(?<=[Ā-ă])x/)[0]
  assert_equal "x", "ax".match(/(?<=[aĀ])x/)[0]
  assert_equal "x", "Āx".match(/(?<=[aĀ])x/)[0]
  assert_equal "x", "ĀĀx".match(/(?<=[Ā]{2})x/)[0]
  assert_nil "aĀx".match(/(?<=[Ā]{2})x/)
  assert_nil "Āx".match(/(?<=[Ā]{2})x/)
  # a negated class admits non-ASCII, whatever its members are
  assert_nil "あx".match(/(?<=[^あ])x/)
  assert_equal "x", "ax".match(/(?<=[^あ])x/)[0]
  assert_nil "Āb".match(/(?<![Ā])b/)
  assert_equal "b", "ab".match(/(?<![Ā])b/)[0]
  assert_equal "b", "ab".match(/(?<![^a])b/)[0]
  assert_nil "あb".match(/(?<![^a])b/)
  # the uppercase shorthands carry the same catch-all
  assert_equal "x", "aあx".match(/(?<=a\W)x/)[0]
  assert_nil "aax".match(/(?<=a\W)x/)
  assert_equal "x", "ああx".match(/(?<=\W\W)x/)[0]
  assert_equal "x", "Āx".match(/(?<=\D)x/)[0]
  assert_equal "x", "Āx".match(/(?<=\S)x/)[0]
  # dot is one character by the same argument
  assert_equal "x", "ax".match(/(?<=.)x/)[0]
  assert_equal "x", "Āx".match(/(?<=.)x/)[0]
  assert_nil "x".match(/(?<=.)x/)
end

assert("Regexp - lookbehind against a binary subject rewinds by bytes") do
  # A binary subject advances one byte at a time, so the same compiled
  # pattern rewinds by its byte count there: two for the literal Ā, and one
  # for a class, which is handed the raw byte as its codepoint. What this
  # contrasts with is the character rewind, which a build reading its strings
  # by byte does not have.
  skip unless __ENCODING__ == "UTF-8"
  bin = "Āx".b
  assert_equal "x", bin.match(/(?<=Ā)x/)[0]
  assert_nil bin.match(/(?<=[Ā])x/)
  assert_equal "x", bin.match(/(?<=[\x80])x/)[0]
  assert_equal "x", bin.match(/(?<=.)x/)[0]
end

assert("Regexp - lookbehind measures bytes that spell no character") do
  # A byte no lead byte reaches is a character of its own, which is what the
  # rewind steps back over, so the width has to count it as one. Counting the
  # lead bytes of the run alone made such a byte part of the character before
  # it, rewound too little, and the lookbehind then failed on text it
  # describes, or succeeded where the negative form describes it.
  #
  # A subject whose bytes spell no character is refused wherever an encoding
  # reads them, so the character rewind is asked of the build that reads none,
  # and the byte rewind below puts the same question to the same bytes in
  # either build.
  if __ENCODING__ == "UTF-8"
    assert_raise(ArgumentError) { "\x80ab" =~ /(?<=\x80a)b/ }
    assert_raise(ArgumentError) { "\x80ab" =~ /(?<!\x80a)b/ }
    assert_raise(ArgumentError) { "\xE3\x81ab" =~ /(?<=\xE3\x81a)b/ }
    assert_raise(ArgumentError) { "\x80ab" =~ Regexp.new("(?<=\x80a)b") }
  else
    assert_equal 2, ("\x80ab" =~ /(?<=\x80a)b/)
    assert_nil ("\x80ab" =~ /(?<!\x80a)b/)
    # a sequence cut short spells no character either, so each of its bytes is
    # one: E3 leads three bytes and only two follow it here
    assert_equal 3, ("\xE3\x81ab" =~ /(?<=\xE3\x81a)b/)
    # the same bytes written into the pattern rather than escaped
    assert_equal 2, ("\x80ab" =~ Regexp.new("(?<=\x80a)b"))
  end
  # a subject that spells characters throughout is read in either build
  assert_nil ("ab" =~ /(?<=\x80a)b/)
  # a byte-indexed subject counts the same bytes, and rewinds by them
  assert_equal 2, ("\x80ab".b =~ Regexp.new("(?<=\x80a)b"))
  assert_equal 3, ("\xE3\x81ab".b =~ Regexp.new("(?<=\xE3\x81a)b"))
  # a whole character is still one whatever its byte count
  assert_equal "b", "Āab".match(/(?<=Āa)b/)[0]
  assert_nil "aab".match(/(?<=Āa)b/)
end

assert("Regexp - lookbehind over a class holding a byte that spells no character") do
  # Two things meet here that were built apart: a character class may hold a
  # byte that starts no character, and the rewind steps back over characters.
  # They agree on the unit already: such a byte is a character of its own,
  # which is the step the forward match takes for it too, so a class holding
  # one is measured as one character wide, the same as any other class.
  #
  # A subject whose bytes spell no character is refused wherever an encoding
  # reads them, so the stray byte is put to a binary subject, which rewinds by
  # bytes in either build.
  assert_equal "x", "\xB5x".b.match(/(?<=[\xB5])x/)[0]
  assert_equal 1, ("\xB5x".b =~ /(?<=[\xB5])x/)
  if __ENCODING__ == "UTF-8"
    assert_raise(ArgumentError) { "\xB5x" =~ /(?<=[\xB5])x/ }
    # the byte written into a class is still asked about a character: Ā is
    # C4 80, and the rewind steps back over the whole of it, so the class is
    # handed U+0100 rather than either of its bytes
    assert_nil ("Āx" =~ /(?<=[\x80])x/)
    assert_nil ("Āx" =~ /(?<=[\xC4])x/)
    assert_equal 1, ("Āx" =~ /(?<=[Ā])x/)
  else
    # a build that reads its strings by byte has one character per byte, so
    # the same class does see the continuation byte, and only that one
    assert_equal 1, ("\xB5x" =~ /(?<=[\xB5])x/)
    assert_equal 2, ("\xC4\x80x" =~ /(?<=[\x80])x/)
    assert_nil ("\xC4\x80x" =~ /(?<=[\xC4])x/)
  end
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

assert("Regexp - a relocated lookaround keeps the end of its sub-pattern") do
  # A lookaround holds the end of its sub-pattern as an absolute code index,
  # so every relocation has to carry it the way it carries a jump target.
  # Neither relocator did: the stale index landed on the sub-pattern's own
  # RE_MATCH, which ends the outer match early, so the answers below flipped
  # in both directions and the MatchData of an apparent success held nil.
  # Three shapes reach a relocator, one each.

  # insert_inst, via the SPLIT a quantifier puts in front of the group
  assert_nil /(?:(?=a)b)*x/.match("a")
  assert_equal "x", /(?:(?!b)b)*x/.match("ax")[0]

  # emit_atom_copy, via the copies {n,m} makes of the group
  assert_equal "aa", /(?:(?=a)a){2}/.match("aa")[0]
  assert_nil /(?:(?=a)a){2}/.match("ab")

  # insert_inst again, via the SPLIT compile_alt puts in front of branch 0
  # once every branch is compiled
  md = /(?=a)a|z/.match("ax")
  assert_equal 0, md.begin(0)
  assert_equal "a", md[0]

  # the same group without a relocation, which always answered correctly
  assert_equal "ab", /(?:(?=a)ab)+/.match("ab")[0]
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
