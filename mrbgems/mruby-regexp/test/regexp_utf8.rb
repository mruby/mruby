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
  # is stored there. The upper bound here is uncased, which keeps this about
  # the split alone: what /i does with a range whose non-ASCII half has case
  # differs by build and is asserted in ascii_case.rb and unicode_case.rb.
  assert_equal "A", "A".match(/[a-©]/i)[0]
  assert_nil "A".match(/[^a-©]/i)
  assert_equal "©", "©".match(/[a-©]/i)[0]
  # Ranges that stay on one side of the boundary are unaffected.
  assert_equal "b", "b".match(/[a-c]/)[0]
  assert_equal "ą", "ą".match(/[Ā-Đ]/)[0]
  assert_nil "a".match(/[Ā-Đ]/)
end

assert("Regexp - /i does not read a byte above 127 as a character") do
  # A byte that starts no whole character decodes as itself, so the folding
  # path would take a lone 0xB5 for U+00B5 and answer /i for a character the
  # pattern does not hold. A literal compares bytes, with or without /i.
  # The byte reaches the engine through a byte-indexed subject, since a UTF-8
  # subject carrying it is refused before the match runs.
  micro = "\xB5"        # U+00B5 is "\xC2\xB5"; the byte on its own is not it
  assert_equal 0, (Regexp.new(micro, Regexp::IGNORECASE) =~ micro.b)
  assert_nil (Regexp.new(micro, Regexp::IGNORECASE) =~ "\u00B5")
  # A sequence cut short by the end of the pattern reads the same way.
  lead = "\xC3"         # starts a two byte character and never completes one
  assert_equal 0, (Regexp.new(lead, Regexp::IGNORECASE) =~ lead.b)
  assert_nil (Regexp.new(lead, Regexp::IGNORECASE) =~ "\u00E3")
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

assert("Regexp - quantifier on an escaped multibyte literal") do
  # A backslash before a character with no escape meaning is just that
  # character, so \Ā has to be one atom exactly like Ā. The escape path used
  # to emit the lead byte alone and leave the continuation byte to the parse
  # loop, so the quantifier bound to that byte instead.
  # The /.../ spelling cannot show this, because the lexer drops the backslash
  # before the gem sees the pattern: /\Ā/.source is the two bytes of Ā alone.
  # A pattern built at runtime arrives through Regexp.new with the backslash
  # still in it.
  assert_equal 4, Regexp.new("\\Ā+").match("ĀĀ")[0].bytesize
  assert_equal 6, Regexp.new("\\ĀĀĀ").match("ĀĀĀ")[0].bytesize
  assert_true Regexp.new("\\Ā{2}").match?("ĀĀ")
  assert_false Regexp.new("\\Ā{2}").match?("Ā")
  assert_equal 6, Regexp.new("\\日+").match("日日")[0].bytesize
  assert_equal 8, Regexp.new("\\𝕏+").match("𝕏𝕏")[0].bytesize
  assert_equal 5, Regexp.new("a\\Ā+").match("aĀĀ")[0].bytesize
  assert_equal 2, Regexp.new("\\Ā+?").match("ĀĀ")[0].bytesize
  # Inside [...] the same escape has to read as one codepoint, or the class
  # holds the lead byte and the continuation byte as two wrong members.
  assert_true Regexp.new("[\\Ā]").match?("Ā")
  assert_false Regexp.new("[\\Ā]").match?("Ä")
  assert_true Regexp.new("[\\Ā-\\ā]").match?("ā")
  assert_false Regexp.new("[\\Ā-\\ā]").match?("Ă")
  # A raw byte escape names a byte rather than a character, so it keeps taking
  # the parse_escape path and the quantifier binds to that one byte. CRuby
  # joins byte escapes that spell a valid UTF-8 sequence into one character
  # and matches four bytes here; closing that gap is a separate change.
  assert_equal 2, Regexp.new("\\xC4\\x80+").match("ĀĀ")[0].bytesize
end

assert("Regexp - quantifier on an invalid multibyte literal") do
  # A byte above 127 is one atom only while it starts a whole character. The
  # sequences below never complete one, so each byte stands alone and the
  # quantifier binds to the byte in front of it, not to the pair.
  lead2 = "\xC4"  # starts a two byte character
  lead3 = "\xE3"  # starts a three byte character
  cont = "\x81"   # continuation byte

  # What the quantifier binds to is settled when the pattern is compiled, so
  # the subjects below are byte-indexed: a UTF-8 subject holding these bytes
  # is refused before the match runs.
  # "x" is not a continuation byte, so `+` repeats "x".
  assert_equal 4, (lead2 + "xxx").b.match(Regexp.new(lead2 + "x+"))[0].bytesize
  assert_equal 4, (lead3 + "abb").b.match(Regexp.new(lead3 + "ab+"))[0].bytesize
  # The quantifier itself must not be taken for a continuation byte either.
  assert_equal 2, (lead2 + lead2).b.match(Regexp.new(lead2 + "+"))[0].bytesize
  # A sequence cut short by the end of the pattern emits its bytes one by one.
  assert_equal 2, (lead3 + cont).b.match(Regexp.new(lead3 + cont))[0].bytesize
  assert_equal 3, (lead3 + cont + cont).b.match(Regexp.new(lead3 + cont + "+"))[0].bytesize
  # A valid character right after an invalid lead byte is still one atom.
  assert_equal 5, (lead2 + "ĀĀ").b.match(Regexp.new(lead2 + "Ā+"))[0].bytesize
  # The subject side reads the same way: `.` takes the lead byte alone.
  assert_equal 1, (lead2 + "x").match(/./)[0].bytesize
  assert_equal 2, "Ā".match(/./)[0].bytesize
end

assert("Regexp - a byte that belongs to no character is a match position") do
  # A byte in 0x80-0xBF is the interior of a character only while a lead byte
  # in front of it reaches that far. One that stands on its own is a boundary
  # like any other, and the engines used to disagree about it: the literal
  # fast path matched there, the NFA never started a match there. A UTF-8
  # subject carrying such a byte is refused before the match runs, so the
  # subjects below are byte-indexed.
  b = "\x81"
  assert_equal 0, (b + b).b.match(Regexp.new(b + b)).begin(0)
  assert_equal 2, (b + b).b.match(Regexp.new(b + "+"))[0].bytesize
  assert_equal 2, (b + b).b.match(Regexp.new(b + "*"))[0].bytesize
  assert_equal 1, (b + b).b.match(Regexp.new(b + "?"))[0].bytesize
  assert_equal 1, ("x" + b + b).b.match(Regexp.new(b + "+")).begin(0)
  # Inside a character there is still no match position, and that subject is
  # whole UTF-8, so it reaches the engine as it always did.
  assert_nil "あ".match(Regexp.new("\x81"))
  assert_nil "あ".match(Regexp.new("\x82"))
  assert_nil "\u{1D54F}".match(Regexp.new("\x95"))
  # Next to one there is.
  assert_equal 0, (b + "あ").b.match(Regexp.new(b)).begin(0)
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
  # And so is a byte where no lead byte reaches it, through a class that holds
  # the byte. [µ] holds the character, whose trailing byte alone is not it.
  assert_equal 2, ("x" + "\xb5").b.match(Regexp.new(".?[\xb5]"))[0].bytesize
  assert_nil ("x" + "\xb5").match(/.?[µ]/)
end

assert("Regexp - a byte-indexed subject is reported in bytes") do
  # `String#b` marks the subject byte-indexed, and MatchData snapshots it with
  # a copy. The copy came back as if it were UTF-8, so #begin counted the
  # characters of a string that has none, and disagreed with #pre_match, which
  # counts the same span in bytes.
  s = "\u{1F600}".b  # F0 9F 98 80: four bytes, one character
  assert_equal 3, (s =~ Regexp.new("\x80"))
  assert_equal 3, s.byteindex(Regexp.new("\x80"))
  md = s.match(Regexp.new("\x80"))
  assert_equal 3, md.begin(0)
  assert_equal 4, md.end(0)
  assert_equal md.pre_match.bytesize, md.begin(0)
  # the same subject read as UTF-8 counts characters, as it always has
  u = "\u{1F600}"
  assert_equal 0, (u =~ /./)
  assert_equal 1, (("x" + u) =~ Regexp.new("\u{1F600}"))
end

assert("Regexp - match positions on malformed UTF-8 agree with string indexing") do
  # String indexing counts a byte no lead byte reaches as one character, but
  # #begin used to count lead bytes only, so a stray continuation byte was
  # zero width to it. Computing the length marks such a string single-byte
  # and switches it to byte counting, so the same match reported one
  # position before the length was known and another after.
  s = "a\x80b"
  m = /b/.match(s)
  before = m.begin(0)
  assert_equal 3, s.length
  assert_equal before, /b/.match(s).begin(0)
  assert_equal 2, before
  assert_equal "b", s[before]
  assert_equal 3, m.end(0)
  # A position argument walks the same characters, from either end. The
  # fresh literal pins the walk on a string whose length is not known yet.
  assert_equal "b", /b/.match(s, 2)[0]
  assert_equal "a", /a/.match(s, -3)[0]
  assert_nil /a/.match(s, -4)
  assert_equal "a", /a/.match("a\x80b", -3)[0]
  # Read as bytes the same subject reports byte offsets, which its own
  # indexing agrees with too.
  b = "a\x80b".b
  bm = /b/.match(b)
  assert_equal 2, bm.begin(0)
  assert_equal 3, bm.end(0)
  assert_equal "b", b[bm.begin(0)]
  assert_equal "b", /b/.match(b, 2)[0]
  assert_equal "a", /a/.match(b, -3)[0]
  assert_nil /a/.match(b, -4)
end

assert("Regexp - a match does not end inside a character") do
  # A pattern is compiled byte by byte and RE_CHAR consumes one byte, so a
  # pattern holding a byte that reaches no character ends its match in the
  # middle of one. "ĵ" is C4 B5, and a pattern of the single byte C4 used to
  # match its lead byte alone and hand back half a character.
  j = "ĵ"
  assert_nil j.match(Regexp.new("\xc4"))          # literal fast path
  assert_nil ("x" + j).match(Regexp.new("\xc4"))
  assert_nil j.match(Regexp.new("\xc4+"))         # pike VM
  assert_nil j.match(Regexp.new("(\xc4)\\1?"))    # backtracking engine
  assert_equal j.bytes, j.gsub(Regexp.new("\xc4"), "!").bytes
  # A branch that does end on a boundary still matches, greedy or not.
  assert_equal 2, j.match(Regexp.new("\xc4."))[0].bytesize
  assert_equal 2, j.match(Regexp.new("\xc4(?:\xb5)?"))[0].bytesize
  assert_equal 2, j.match(Regexp.new("\xc4(?:\xb5)??"))[0].bytesize
  assert_equal 2, j.match(Regexp.new("(\xc4\xb5)\\1?"))[0].bytesize
  assert_equal 0, j.match(Regexp.new("\xc4*"))[0].bytesize
  # A lookaround ends at a position without consuming it, so it is not the
  # end of the match and keeps its own answer.
  assert_equal 2, j.match(Regexp.new("(?=\xc4)\xc4\xb5"))[0].bytesize
  # A byte no lead byte reaches is a boundary, so a byte pattern still works.
  # Such a subject is refused as UTF-8, so it is byte-indexed here.
  b = "\x81"
  assert_equal 1, (b + b).b.match(Regexp.new(b))[0].bytesize
  assert_equal 2, (b + b).b.match(Regexp.new(b + "+"))[0].bytesize
  assert_equal 1, ("a" + b).b.match(Regexp.new(b))[0].bytesize
  # Read as binary every position is a boundary, so nothing changes there.
  if Object.const_defined?(:Encoding)
    bin = j.dup.force_encoding("ASCII-8BIT")
    assert_equal 1, bin.match(Regexp.new("\xc4"))[0].bytesize
    assert_equal 2, bin.match(Regexp.new("\xc4."))[0].bytesize
  end
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
  # The position one past the last character is the end, not out of range.
  assert_equal 3, //.match("あいあ", 3).begin(0)
  assert_true /あ/.match?("あいあ", 2)
  assert_false /い/.match?("あいあ", 2)
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

assert("Regexp - \\u escapes") do
  # `\u` used to be unknown to the engine, which dropped the backslash and
  # left the rest as literal text: /\u00b5/ matched "u00b5" rather than "µ".
  assert_equal 0, (/\u00b5/ =~ "\xc2\xb5")
  assert_nil (/\u00b5/ =~ "u00b5")
  assert_equal 0, (/\u{b5}/ =~ "\xc2\xb5")
  assert_nil (/\u{b5}/ =~ "u{b5}")
  assert_equal 0, (/\u0061/ =~ "a")
  assert_equal 0, (/\u{3042}/ =~ "\xe3\x81\x82")
  assert_equal 0, (/\u{10FFFF}/ =~ "\xf4\x8f\xbf\xbf")

  # a codepoint is one atom, so a quantifier repeats the whole character
  # rather than its last UTF-8 byte
  assert_equal ["\xe3\x81\x82\xe3\x81\x82"], "\xe3\x81\x82\xe3\x81\x82".scan(/\u{3042}+/)
  assert_equal 0, (/\u{3042}{2}/ =~ "\xe3\x81\x82\xe3\x81\x82")

  # the list form is a sequence of atoms, so a following quantifier binds
  # to the last codepoint alone: /\u{61 62}+/ is `ab+`
  assert_equal "ab", "abbb"[/\u{61 62}/]
  assert_equal "abbb", "abbb"[/\u{61 62}+/]
  assert_nil ("b" =~ /\u{61 62}/)

  # /x strips whitespace before the pattern is parsed, but not the spaces
  # that separate the codepoints of a list
  assert_equal 0, (Regexp.new("\\u{61 62}", Regexp::EXTENDED) =~ "ab")

  # /i folds an ASCII letter reached through `\u`, like a literal one
  assert_equal 0, (/\u0061/i =~ "A")
end

assert("Regexp - \\u escapes in a character class") do
  assert_equal 0, (/[\u00b5]/ =~ "\xc2\xb5")
  assert_nil (/[\u00b5]/ =~ "u")
  assert_equal 0, (/[\u{3042}-\u{3044}]/ =~ "\xe3\x81\x83")
  assert_nil (/[\u{3042}-\u{3044}]/ =~ "\xe3\x81\x85")
  assert_equal 0, (/[a-\u{7a}]/ =~ "q")

  # every codepoint of a list is a member of its own, and the last one can
  # still open a range
  assert_equal ["a", "b"], "abc".scan(/[\u{61 62}]/)
  assert_equal ["a", "b", "c"], "abc-".scan(/[\u{61 62}-z]/)
  assert_equal ["c"], "abc".scan(/[^\u{61 62}]/)
end

assert("Regexp - malformed \\u escapes") do
  # each of these is a RegexpError in CRuby rather than a shorter codepoint
  # or literal text
  assert_raise_with_message(RegexpError, "invalid Unicode escape: /\\uXX/") do
    Regexp.new("\\uXX")
  end
  assert_raise_with_message(RegexpError, "too short escape sequence: /\\u/") do
    Regexp.new("\\u")
  end
  assert_raise(RegexpError) { Regexp.new("\\u061") }       # fewer than four digits
  assert_raise(RegexpError) { Regexp.new("\\u{}") }        # empty list
  assert_raise(RegexpError) { Regexp.new("\\u{ }") }       # list of no codepoints
  assert_raise(RegexpError) { Regexp.new("\\u{61") }       # unterminated list
  assert_raise(RegexpError) { Regexp.new("\\u{61,62}") }   # comma is not a separator
  assert_raise(RegexpError) { Regexp.new("\\u{0000061}") } # more than six digits
  assert_raise(RegexpError) { Regexp.new("\\uD800") }      # surrogate
  assert_raise(RegexpError) { Regexp.new("[\\u{110000}]") }

  # /\u{3042}/ used to be read as the quantifier `u{3042}`, so a codepoint
  # out of range was reported as a repeat count the pattern never wrote
  assert_raise_with_message(RegexpError, "invalid Unicode range: /\\u{110000}/") do
    Regexp.new("\\u{110000}")
  end
end

assert("Regexp - invalid UTF-8 byte near pattern end") do
  # a truncated multi-byte leader in a character class must not read
  # past the end of the pattern buffer
  re = Regexp.new("[   \xff ]")
  assert_kind_of Regexp, re
  assert_equal 0, (re =~ "\xff".b)
  assert_nil (re =~ "x")
end

assert("Regexp - truncated UTF-8 at subject end") do
  # a lone multi-byte leader at the end of the subject must not read
  # past the end of the string buffer when matched against a class.
  # Byte-indexed as well, where the engine reads the leader as a byte of its
  # own and the walk past it is a different one.
  assert_nil ("ab\xf0" =~ /[cd]/)
  assert_equal 0, ("ab\xf0" =~ /[^cd]+$/)
  assert_nil ("ab\xf0".b =~ /[cd]/)
  assert_equal 0, ("ab\xf0".b =~ /[^cd]+$/)
end

assert("Regexp - overlong UTF-8 is not the character it spells") do
  # C0 BC is the two-byte overlong spelling of "<" and E0 84 80 the three-byte
  # spelling of "Ā". A decoder that hands out a codepoint for these would let a
  # class hold a character the subject does not spell, so assert the class and
  # the literal together against the same subject.
  assert_nil ("\xC0\xBC" =~ /[<]/)
  assert_nil ("\xC0\xBC" =~ /</)
  assert_equal 0, ("\xC0\xBC" =~ /[^<]/)
  assert_equal "\xC0\xBC", "\xC0\xBC".gsub(/[<]/, "&lt;")
  assert_nil ("\xE0\x80\xBC" =~ /[<]/)
  assert_false Regexp.new("[Ā]").match?("\xE0\x84\x80")
  assert_false (/Ā/.match?("\xE0\x84\x80"))
  # surrogates and codepoints above U+10FFFF encode no character either, so
  # each byte stands on its own
  assert_equal 2, "\xC0\xBC".scan(/./).size
  assert_equal 3, "\xED\xA0\x80".scan(/./).size
  assert_equal 4, "\xF0\x80\x80\xBC".scan(/./).size
  assert_equal 4, "\xF4\x90\x80\x80".scan(/./).size
  assert_equal 4, "\xF5\x80\x80\x80".scan(/./).size
  # Byte-indexed the same bytes stand on their own too.
  assert_equal 2, "\xC0\xBC".b.scan(/./).size
  assert_equal 3, "\xED\xA0\x80".b.scan(/./).size
  assert_equal 4, "\xF0\x80\x80\xBC".b.scan(/./).size
  assert_equal 4, "\xF4\x90\x80\x80".b.scan(/./).size
  assert_equal 4, "\xF5\x80\x80\x80".b.scan(/./).size
  # the pattern side decodes through the same helper
  assert_false Regexp.new("[\xC0\xBC]").match?("<")
  # the shortest spelling on each side of those bounds is still one character
  assert_equal 1, "\u{0080}".scan(/./).size    # C2 80
  assert_equal 1, "\u{0800}".scan(/./).size    # E0 A0 80
  assert_equal 1, "\u{D7FF}".scan(/./).size    # ED 9F BF
  assert_equal 1, "\u{E000}".scan(/./).size    # EE 80 80
  assert_equal 1, "\u{10000}".scan(/./).size   # F0 90 80 80
  assert_equal 1, "\u{10FFFF}".scan(/./).size  # F4 8F BF BF
  assert_equal 0, ("\u{0800}" =~ Regexp.new("[\u{0800}]"))
  assert_equal 0, ("\u{10FFFF}" =~ Regexp.new("[\u{10FFFF}]"))
end

assert("Regexp - a pattern byte that starts no character is a byte in a class") do
  # A class used to read a lone continuation byte as the codepoint of its
  # number, so "[\xB5]" held U+00B5 while "\xB5" held the byte: one pattern
  # meant two things depending on which side of the brackets it was written.
  # CRuby settles it with the pattern's encoding and raises RegexpError for
  # either spelling; this gem has no encoding to consult, so it reads the byte
  # as the byte on both sides.
  mu = "\xC2\xB5"  # U+00B5 MICRO SIGN, two bytes
  assert_nil (mu =~ Regexp.new("[\xB5]"))
  assert_nil (mu =~ Regexp.new("\xB5"))
  assert_equal mu.bytes, mu.gsub(Regexp.new("[\xB5]"), "!").bytes
  assert_equal mu.bytes, mu.gsub(Regexp.new("\xB5"), "!").bytes
  assert_equal 0, ("\xB5".b =~ Regexp.new("[\xB5]"))     # the byte alone is it
  assert_equal 1, (mu.b =~ Regexp.new("[\xB5]"))         # so is a byte subject
  assert_equal 0, (mu =~ Regexp.new("[^\xB5]"))
  # An escape names a byte too, which is what the literal path emits for it.
  assert_nil (mu =~ Regexp.new("[\\xB5]"))
  assert_equal 0, ("\xB5".b =~ Regexp.new("[\\xB5]"))
  # `\u` names a codepoint outright, so it is how the character gets spelled
  # where the byte of the same number will not do.
  assert_equal 0, (mu =~ Regexp.new("[\\u{B5}]"))
  assert_nil ("\xB5".b =~ Regexp.new("[\\u{B5}]"))
  # An invalid leader is a byte on both sides for the same reason, which is
  # what "overlong UTF-8 is not the character it spells" pins for the class.
  assert_equal 0, ("\xC0".b =~ Regexp.new("[\xC0]"))
  assert_nil ("À" =~ Regexp.new("[\xC0]"))               # C3 80
  # A byte range is how a continuation byte gets spelled, and it stays a range
  # of bytes: it holds no character of its own.
  data = "\xC2\xB5A\xCE\xBC"
  assert_equal 2, data.b.scan(Regexp.new("[\x80-\xBF]")).size
  assert_equal 0, data.scan(Regexp.new("[\x80-\xBF]")).size
  assert_equal 0, ("\u{00BF}" =~ Regexp.new("[^\x80-\xBF]"))
  # A range from a byte to a character names neither, however it is spelled.
  assert_raise(RegexpError) { Regexp.new("[\x80-µ]") }
  assert_raise(RegexpError) { Regexp.new("[µ-\x80]") }
  assert_raise(RegexpError) { Regexp.new("[\\u{B5}-\\xBF]") }
  # ASCII belongs to both, so it pairs with either.
  assert_equal 0, ("\xFF".b =~ Regexp.new("[\x00-\xFF]"))
  assert_equal 0, ("µ" =~ Regexp.new("[\x00-\u{FF}]"))
end

assert("Regexp - /i over a class of bytes asks for no case data") do
  # Folding is for characters, and a byte that starts none has no case: a
  # class of continuation bytes used to reach the fold tables through the
  # codepoint its number spells, which refused the pattern on a build without
  # them and folded it into two Greek letters on a build with them.
  assert_kind_of Regexp, Regexp.new("[\xB5]", Regexp::IGNORECASE)
  assert_kind_of Regexp, Regexp.new("[\x80-\xBF]", Regexp::IGNORECASE)
  assert_kind_of Regexp, Regexp.new("[\xC0\xBC]", Regexp::IGNORECASE)
  assert_nil ("μ" =~ Regexp.new("[\xB5]", Regexp::IGNORECASE))
  assert_nil ("Μ" =~ Regexp.new("[\xB5]", Regexp::IGNORECASE))
  assert_equal 0, ("μ" =~ Regexp.new("[^\xB5]", Regexp::IGNORECASE))
  assert_equal 2, "\xC2\xB5A\xCE\xBC".b.scan(Regexp.new("[\x80-\xBF]", Regexp::IGNORECASE)).size
  # The characters in the same class still fold.
  assert_equal 0, ("K" =~ Regexp.new("[\x80-\xBF k]", Regexp::IGNORECASE))
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
