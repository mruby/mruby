# What a run of bytes spells is a question only a build that reads UTF-8 can
# answer: core carries the scan behind MRB_UTF8_STRING, and a build without it
# reads a String one byte per character, so the engine reads pattern and
# subject that way too. Every block below that puts that question skips there,
# whether it puts it to a subject read as UTF-8 or through a pattern spelling
# a character in more than one byte. What holds on either build, a
# byte-indexed subject among it, runs unguarded.

assert("Regexp - dot advances by string mode") do
  skip unless __ENCODING__ == "UTF-8"
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
  skip unless __ENCODING__ == "UTF-8"
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
  skip unless __ENCODING__ == "UTF-8"
  micro = "\xB5"        # U+00B5 is "\xC2\xB5"; the byte on its own is not it
  assert_nil (Regexp.new(micro, Regexp::IGNORECASE) =~ "\u00B5")
  # A sequence cut short by the end of the pattern reads the same way.
  lead = "\xC3"         # starts a two byte character and never completes one
  assert_nil (Regexp.new(lead, Regexp::IGNORECASE) =~ "\u00E3")
  # What /i folds is settled when the pattern is compiled, so a byte-indexed
  # subject puts the same question to the engine, which reads one through a
  # branch of its own: `mrb_re_exec` takes the flag and every step it drives,
  # the fold included, turns on it.
  #
  # It is also the only subject that can answer, which is why this block and
  # the ones below ask no UTF-8 subject about a byte like this. Such a subject
  # spells no character where the byte is, so what the engine does with it is
  # whatever a decoder happens to do with input no rule covers, and an
  # assertion on one pins that accident rather than the pattern.
  assert_equal 0, (Regexp.new(micro, Regexp::IGNORECASE) =~ micro.b)
  assert_equal 0, (Regexp.new(lead, Regexp::IGNORECASE) =~ lead.b)
end

assert("Regexp - /i has no character to fold in a byte-indexed pattern") do
  # A byte-indexed pattern holds bytes, so there is nothing in it for /i to
  # fold even where the bytes do spell a character. Folding one emits a class,
  # and a class compares a decoded character, which a byte-indexed subject
  # never hands it: the two bytes of U+00B5 stopped matching the same two
  # bytes. Read as bytes they are two atoms, so a quantifier after them
  # repeats the last, the same reading the escaped spelling of them takes.
  mu = "\xC2\xB5"       # the two bytes of U+00B5
  assert_equal 0, (Regexp.new(mu.b, Regexp::IGNORECASE) =~ mu.b)
  assert_equal 2, Regexp.new(mu.b, Regexp::IGNORECASE).match(mu.b)[0].bytesize
  assert_equal 3, Regexp.new((mu + "+").b, Regexp::IGNORECASE).match((mu + "\xB5").b)[0].bytesize
  assert_equal 3, Regexp.new("\\xC2\\xB5+".b, Regexp::IGNORECASE).match((mu + "\xB5").b)[0].bytesize
  # The bytes of a character with an ASCII counterpart read the same way: what
  # /i would fold is the character, and the pattern does not hold one.
  kelvin = "\xE2\x84\xAA"  # U+212A KELVIN SIGN, which folds to "k"
  assert_nil (Regexp.new(kelvin.b, Regexp::IGNORECASE) =~ "k")
  assert_equal 0, (Regexp.new(kelvin.b, Regexp::IGNORECASE) =~ kelvin.b)
end

assert("Regexp - a backreference under /i folds a byte-indexed subject by ASCII") do
  need_backtracking_stack
  # A byte-indexed subject hands the folded comparison bytes, and a byte above
  # 127 is not the codepoint of the same value: 0xC0 is not U+00C0. The
  # comparison folded it as if it were, so a build with the Unicode table
  # paired 0xC0 with 0xE0 the way it pairs "À" with "à". The letters a byte
  # can spell are the ASCII ones, and those still fold.
  assert_nil ("\xC0\xE0".b =~ /(.)\1/i)
  assert_nil ("\xC0a\xE0A".b =~ /(..)\1/i)
  assert_equal 0, ("\xC0\xC0".b =~ /(.)\1/i)
  assert_equal 0, ("\xC0a\xC0A".b =~ /(..)\1/i)
  assert_equal 0, ("aA".b =~ /(.)\1/i)
  # Nor does a byte take the fold of the character it is part of. Read as
  # characters "s" and "ſ" fold alike (U+017F to 's', which every build
  # carries), and the same bytes read one at a time have no character to fold.
  # A skip here would drop the assertions above, so this is a branch.
  if __ENCODING__ == "UTF-8"
    assert_equal 0, ("sſ" =~ /(.)\1/i)
    assert_nil ("sſ".b =~ /(.)\1/i)
  end
end

assert("Regexp - quantifier on a multibyte literal") do
  # The bytes of a multibyte literal used to be separate atoms, so a
  # quantifier bound to the last one: /Ā+/ was \xC4(\x80)+ and stopped after
  # one Ā. The byte counts below are what tells the two apart.
  skip unless __ENCODING__ == "UTF-8"
  assert_equal 4, "ĀĀ".match(/Ā+/)[0].bytesize
  assert_equal 4, "ĀĀ".match(/Ā*/)[0].bytesize
  assert_equal 6, "ĀĀĀ".match(/Ā{2,3}/)[0].bytesize
  assert_true "ĀĀ".match?(/Ā{2}/)
  assert_false "Ā".match?(/Ā{2}/)
  # Three and four byte characters take the same path.
  assert_equal 6, "日日".match(/日+/)[0].bytesize
  assert_equal 8, "𝕏𝕏".match(/𝕏+/)[0].bytesize
  # A quantified literal after another atom.
  assert_equal 5, "aĀĀ".match(/aĀ+/)[0].bytesize
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
  skip unless __ENCODING__ == "UTF-8"
  assert_equal 4, Regexp.new("\\Ā+").match("ĀĀ")[0].bytesize
  assert_equal 6, Regexp.new("\\ĀĀĀ").match("ĀĀĀ")[0].bytesize
  assert_true Regexp.new("\\Ā{2}").match?("ĀĀ")
  assert_false Regexp.new("\\Ā{2}").match?("Ā")
  assert_equal 6, Regexp.new("\\日+").match("日日")[0].bytesize
  assert_equal 8, Regexp.new("\\𝕏+").match("𝕏𝕏")[0].bytesize
  assert_equal 5, Regexp.new("a\\Ā+").match("aĀĀ")[0].bytesize
  # Inside [...] the same escape has to read as one codepoint, or the class
  # holds the lead byte and the continuation byte as two wrong members.
  assert_true Regexp.new("[\\Ā]").match?("Ā")
  assert_false Regexp.new("[\\Ā]").match?("Ä")
  assert_true Regexp.new("[\\Ā-\\ā]").match?("ā")
  assert_false Regexp.new("[\\Ā-\\ā]").match?("Ă")
  # Byte escapes that spell a character are that character, so the quantifier
  # binds to the whole of it here too, as CRuby's does. The bytes are read the
  # same way however the pattern spells them.
  assert_equal 4, Regexp.new("\\xC4\\x80+").match("ĀĀ")[0].bytesize
  assert_equal 4, Regexp.new("\\xC4\\x80\\xC4\\x80").match("ĀĀ")[0].bytesize
end

assert("Regexp - a non-greedy quantifier on a multibyte literal binds to the character") do
  need_backtracking_stack
  # The two spellings above, asked to stop as early as they may. A non-greedy
  # repeat is what the Pike VM cannot run, so these are the assertions of the
  # two blocks that stand on the backtracking stack; the rest run at any
  # stack limit.
  skip unless __ENCODING__ == "UTF-8"
  assert_equal 2, "ĀĀ".match(/Ā+?/)[0].bytesize
  assert_equal 2, Regexp.new("\\Ā+?").match("ĀĀ")[0].bytesize
end

assert("Regexp - quantifier on an invalid multibyte literal") do
  # A byte above 127 is one atom only while it starts a whole character. The
  # sequences below never complete one, so each byte stands alone and the
  # quantifier binds to the byte in front of it, not to the pair. What it binds
  # to is settled when the pattern is compiled, so the subjects carrying these
  # bytes are byte-indexed, where each of them is a byte and the byte counts
  # they report are offsets into what was asked.
  skip unless __ENCODING__ == "UTF-8"
  lead2 = "\xC4"  # starts a two byte character
  lead3 = "\xE3"  # starts a three byte character
  cont = "\x81"   # continuation byte

  # A whole character is one atom on the subject side, which is what a subject
  # read as UTF-8 is left to answer here.
  assert_equal 2, "Ā".match(/./)[0].bytesize
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
  assert_equal 1, (lead2 + "x").b.match(/./)[0].bytesize
end

assert("Regexp - a byte that belongs to no character is a match position") do
  # A byte in 0x80-0xBF is the interior of a character only while a lead byte
  # in front of it reaches that far. One that stands on its own is a boundary
  # like any other, and the engines used to disagree about it: the literal
  # fast path matched there, the NFA never started a match there. Where such a
  # byte opens a match position is a question about the byte, put to a
  # byte-indexed subject because one read as UTF-8 spells no character there to
  # answer for it. That also makes #begin a byte offset, which is what
  # pre_match used to be needed for.
  # Inside a character there is still no match position, which is what a
  # subject read as UTF-8 is left to answer.
  skip unless __ENCODING__ == "UTF-8"
  assert_nil "あ".match(Regexp.new("\x81"))
  assert_nil "あ".match(Regexp.new("\x82"))
  assert_nil "\u{1D54F}".match(Regexp.new("\x95"))
  b = "\x81"
  assert_equal 0, (b + b).b.match(Regexp.new(b + b)).begin(0)
  assert_equal 2, (b + b).b.match(Regexp.new(b + "+"))[0].bytesize
  assert_equal 2, (b + b).b.match(Regexp.new(b + "*"))[0].bytesize
  assert_equal 1, (b + b).b.match(Regexp.new(b + "?"))[0].bytesize
  assert_equal 1, ("x" + b + b).b.match(Regexp.new(b + "+")).begin(0)
  # Next to one there is a match position too.
  assert_equal 0, (b + "あ").b.match(Regexp.new(b)).begin(0)
end

assert("Regexp - an attempt in flight opens no match position inside a character") do
  # "ĵ" is C4 B5 and "µ" is C2 B5, so the two share their trailing byte. That
  # byte is the interior of "ĵ" and no match may start there, but the test
  # for it only ran while nothing was in flight. The branch of `.?` that
  # consumes the character parks a thread past it, and the attempt seeded at
  # the shared byte then matched it on its own, cutting "ĵ" in half.
  skip unless __ENCODING__ == "UTF-8"
  assert_nil "ĵ".match(/.?[µ]/)
  assert_nil "ĵ".gsub(/.?[µ]/, "!").match(/!/)
  assert_nil ("あ" + "ĵ").match(/.?[µ]/)
  # A character the class does hold is still found through the same branch.
  assert_equal 4, ("ĵ" + "µ").match(/.?[µ]/)[0].bytesize
  assert_equal 5, ("あ" + "µ").match(/.?[µ]/)[0].bytesize
  # And so is a byte where no lead byte reaches it, through a class that holds
  # the byte. [µ] holds the character, whose trailing byte alone is not it. The
  # subject is byte-indexed, since one read as UTF-8 spells no character at
  # that byte, and the thread `.?` parks past it is stepped by the branch the
  # engine keeps for one. A class that holds the character rather than the byte
  # still does not hold it.
  assert_equal 2, ("x" + "\xb5").b.match(Regexp.new(".?[\xb5]"))[0].bytesize
  assert_nil ("x" + "\xb5").b.match(/.?[µ]/)
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

assert("Regexp - a subject whose bytes are not UTF-8 is refused whatever is known about it") do
  # The refusal must not turn on what the string happens to have been asked
  # about: the single-byte flag `#length` leaves behind says one byte per
  # character, which a string of stray bytes satisfies too, so reading it in
  # place of walking the bytes would make the same call answer one way before
  # a length was taken and another after. That is what #begin used to do here,
  # counting lead bytes only until the flag switched it to counting bytes.
  s = "a\x80b"
  if __ENCODING__ == "UTF-8"
    assert_raise(ArgumentError) { /b/.match(s) }
    assert_equal 3, s.length
    assert_raise(ArgumentError) { /b/.match(s) }
    # A position argument is no way around it either, from either end. The
    # fresh literal puts the question to a string whose length is not known.
    assert_raise(ArgumentError) { /b/.match(s, 2) }
    assert_raise(ArgumentError) { /a/.match("a\x80b", -3) }
  else
    # A build that reads no encoding for these bytes to break has nothing to
    # refuse, and reports the byte positions the half below asserts in either
    # build.
    assert_equal 2, /b/.match(s).begin(0)
    assert_equal 3, s.length
    assert_equal 2, /b/.match(s).begin(0)
    assert_equal "b", /b/.match(s, 2)[0]
    assert_equal "a", /a/.match("a\x80b", -3)[0]
  end
  # A position outside the subject is settled before the bytes are read, so it
  # answers nil in either build rather than raising.
  assert_nil /a/.match(s, -4)
  # Read as bytes the same subject makes no claim that could be broken, and
  # every position it reports is a byte offset its own indexing agrees with,
  # walked from either end.
  bs = "a\x80b".b
  bm = /b/.match(bs)
  assert_equal 2, bm.begin(0)
  assert_equal 3, bm.end(0)
  assert_equal "b", bs[bm.begin(0)]
  assert_equal "b", /b/.match(bs, 2)[0]
  assert_equal "a", /a/.match(bs, -3)[0]
  assert_nil /a/.match(bs, -4)
end

assert("Regexp - a byte that spells no character matches only a byte") do
  # A pattern byte above 127 that starts no whole character is a byte, not the
  # codepoint of the same number, and the byte it names is not the one inside a
  # character that happens to hold that value: "ĵ" is C4 B5 and holds no byte
  # of its own. A class already read such a byte this way; the literal used to
  # match a byte of a character with it and stop the match, a capture or a
  # lookaround between the two halves of one.
  # Every pattern below that holds a byte spelling no character is one CRuby
  # refuses at Regexp.new (`too short escaped multibyte character`) rather
  # than answering, because a Regexp there carries an encoding to refuse with.
  # This gem has none and accepts them, which is what keeps a pattern of a
  # lone byte usable as a way to find that byte. So what these assertions pin
  # is the answer this gem gives while it accepts them, and they are what has
  # to become the raise if a Regexp ever carries an encoding.
  skip unless __ENCODING__ == "UTF-8"
  j = "ĵ"
  assert_nil j.match(Regexp.new("\xc4"))          # literal fast path
  assert_nil ("x" + j).match(Regexp.new("\xc4"))
  assert_nil j.match(Regexp.new("\xc4+"))         # pike VM
  assert_equal j.bytes, j.gsub(Regexp.new("\xc4"), "!").bytes
  # Nothing built on it reaches the character either, where the rule used to be
  # a test on the end of the match and let these through.
  assert_nil j.match(Regexp.new("\xc4."))
  assert_nil j.match(Regexp.new("\xc4(?:\xb5)?"))
  assert_equal 0, j.match(Regexp.new("\xc4*"))[0].bytesize
  # The bytes that do spell the character are the character, so they match it.
  assert_equal 2, j.match(Regexp.new("\xc4\xb5"))[0].bytesize
  # Read as binary every byte stands alone, so every one of them is a byte the
  # pattern can name.
  if Object.const_defined?(:Encoding)
    bin = j.dup.force_encoding("ASCII-8BIT")
    assert_equal 1, bin.match(Regexp.new("\xc4"))[0].bytesize
    assert_equal 2, bin.match(Regexp.new("\xc4."))[0].bytesize
  end
  # A byte no lead byte reaches stands alone in a UTF-8 subject too, so a byte
  # pattern finds it there.
  b = "\x81"
  assert_equal 1, (b + b).b.match(Regexp.new(b))[0].bytesize
  assert_equal 2, (b + b).b.match(Regexp.new(b + "+"))[0].bytesize
  assert_equal 1, ("a" + b).b.match(Regexp.new(b))[0].bytesize
end

assert("Regexp - the backtracking engine reads such a byte the same way") do
  need_backtracking_stack
  # The same question put to the third engine: a backreference or a lookaround
  # is what sends a pattern there, where the block above stays with the
  # literal fast path and the Pike VM and runs at any stack limit.
  skip unless __ENCODING__ == "UTF-8"
  j = "ĵ"
  assert_nil j.match(Regexp.new("(\xc4)\\1?"))
  assert_nil j.match(Regexp.new("(?=\xc4)\xc4\xb5"))
  assert_equal 2, j.match(Regexp.new("(\xc4\xb5)\\1?"))[0].bytesize
end

assert("Regexp - a capture spans whole characters") do
  # A capture is bracketed wherever the pattern brackets it, so where a byte
  # could match inside a character it could close, or open, between the two
  # bytes of one and hand back half of it. The whole match could not, the end
  # of group 0 carrying a rule the other slots did not. On a build that
  # indexes by character that span had no offsets to name it either: m[1] held
  # the lead byte of "Ā" while m.begin(1) and m.end(1) were both the character
  # it sits in. A byte that spells no character now matches only a byte that
  # stands alone, so no group is recorded at a position inside a character.
  # Every pattern below that holds a byte spelling no character is one CRuby
  # refuses at Regexp.new (`too short escaped multibyte character`) rather
  # than answering, because a Regexp there carries an encoding to refuse with.
  # This gem has none and accepts them, which is what keeps a pattern of a
  # lone byte usable as a way to find that byte. So what these assertions pin
  # is the answer this gem gives while it accepts them, and they are what has
  # to become the raise if a Regexp ever carries an encoding.
  skip unless __ENCODING__ == "UTF-8"
  a = "Āx"  # C4 80 78
  assert_nil Regexp.new("(\xc4).").match(a)
  assert_nil Regexp.new("\xc4(\x80)").match(a)
  assert_nil Regexp.new("((\xc4).)").match(a)
  # A capture that does span whole characters matches, and its offsets are the
  # characters it holds.
  m = Regexp.new("(\xc4\x80)(.)").match(a)
  assert_equal [0xc4, 0x80], m[1].bytes
  assert_equal "x", m[2]
  assert_equal [0, 1, 1, 2], [m.begin(1), m.end(1), m.begin(2), m.end(2)]
  assert_equal [[0, 1], [1, 2]], [m.offset(1), m.offset(2)]
  # Read as binary every byte stands alone, so a byte capture works there.
  if Object.const_defined?(:Encoding)
    bm = Regexp.new("(\xc4).").match("\xc4\x80x".b)
    assert_equal [0xc4, 0x80], bm[0].bytes
    assert_equal [0xc4], bm[1].bytes
  end
  # ...and a byte no lead byte reaches stands alone in a UTF-8 subject too.
  b = "\x81"
  bm = Regexp.new("(" + b + ")(" + b + ")").match((b + b).b)
  assert_equal [0x81], bm[1].bytes
  assert_equal [0x81], bm[2].bytes
end

assert("Regexp - a capture the backtracking engine records spans whole characters") do
  need_backtracking_stack
  # A group inside a lookaround, and one a backreference reads back, are
  # recorded by the engine the block above does not reach. The rule is the
  # same there: no group opens or closes inside a character.
  skip unless __ENCODING__ == "UTF-8"
  a = "Āx"  # C4 80 78
  assert_nil Regexp.new("(\xc4).\\1?").match(a)
  assert_nil Regexp.new("(?=(\xc4))\xc4\x80").match(a)
  assert_equal 0, Regexp.new("(?!(\xc4))").match(a).begin(0)
end

assert("Regexp - a lookbehind branch counts its own width in both units") do
  need_backtracking_stack
  # A rewind is counted in characters against a subject read as characters and
  # in bytes against one read as bytes, so a branch carries both counts. With
  # one width per branch there is a pair per branch, and the two branches here
  # disagree in each unit on its own: `ā` is one character and two bytes, `bc`
  # two characters and two bytes, so a rewind that had only the byte count
  # could not tell them apart.
  skip unless __ENCODING__ == "UTF-8"
  assert_equal 1, ("ābx" =~ /(?<=ā|bc)b/)
  assert_equal 2, ("bcbx" =~ /(?<=ā|bc)b/)
  assert_nil ("abx" =~ /(?<=ā|bc)b/)
  # the same pattern against a byte-indexed subject rewinds by the bytes, so
  # both branches step back two of them
  bin = "ābx".b
  assert_equal 2, (bin =~ /(?<=ā|bc)b/)
  assert_nil ("abx".b =~ /(?<=ā|bc)b/)
end

assert("Regexp - a lookaround holds where its sub-pattern matches") do
  need_backtracking_stack
  # A lookaround consumes nothing, so where its sub-pattern stopped was not the
  # end of a match and nothing held it to a character. It could therefore
  # answer the opposite of the body it asserts: at the start of "Ā" the pattern
  # \xC4 finds nothing, yet (?=\xC4) held there and (?!\xC4) did not. The body
  # reaches the same positions as the rest of the search now, so both halves
  # read what they assert.
  # Every pattern below that holds a byte spelling no character is one CRuby
  # refuses at Regexp.new (`too short escaped multibyte character`) rather
  # than answering, because a Regexp there carries an encoding to refuse with.
  # This gem has none and accepts them, which is what keeps a pattern of a
  # lone byte usable as a way to find that byte. So what these assertions pin
  # is the answer this gem gives while it accepts them, and they are what has
  # to become the raise if a Regexp ever carries an encoding.
  skip unless __ENCODING__ == "UTF-8"
  a = "Āx"  # C4 80 78
  assert_nil a.match(Regexp.new("\xc4"))
  assert_nil a.match(Regexp.new("(?=\xc4)"))                # was: held at byte 0
  assert_equal 0, a.match(Regexp.new("(?!\xc4)")).begin(0)   # was: byte 2
  # A lookbehind read the same way, rewound a character and stopping one byte
  # short of the position it was asserting about.
  assert_nil a.match(Regexp.new("(?<=\xc4)"))               # was: held at byte 2
  assert_equal 1, a.match(Regexp.new("(?<!\xc4)x")).begin(0)
  # A body that does match asserts what it always did.
  assert_equal 1, a.match(Regexp.new("(?=x)")).begin(0)
  assert_equal 1, a.match(Regexp.new("(?<=Ā)")).begin(0)
  assert_equal "x", a.match(Regexp.new("(?=(x))"))[1]
  assert_equal "Ā", a.match(Regexp.new("(?<=(Ā))x"))[1]
  assert_equal 2, a.match(Regexp.new("(?=\xc4\x80)\xc4\x80"))[0].bytesize
  # Read as binary the byte is there to assert about.
  if Object.const_defined?(:Encoding)
    assert_equal 0, "\xc4\x80x".b.match(Regexp.new("(?=\xc4)")).begin(0)
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

assert("MatchData#inspect spells the groups by string mode") do
  # The values go through String#inspect, which keeps a UTF-8 character
  # whole only on a build that reads them
  skip unless __ENCODING__ == "UTF-8"
  assert_equal %(#<MatchData "あ" 1:"あ" 2:nil>), /(あ)(x)?/.match("あ").inspect
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

assert("Regexp - a class holds the union of its ranges however they are written") do
  # The ranges are held sorted and free of overlaps, so writing one inside
  # another, writing a pair the wrong way round, or naming the same member
  # twice all come to the class the union spells once. What it takes for that
  # to hold is a search: only the range written last used to be widened, so
  # anything written out of order was kept as a second entry naming what the
  # first already accepted.
  skip unless __ENCODING__ == "UTF-8"
  [/[Ā-Ȁ]/, /[ƀ-ȀĀ-Ɛ]/,
   /[Ā-Őő-Ȁ]/, /[Ā-ȀĠ-İ]/,
   /[ȀĀ-ȀĀ]/].each do |re|
    assert_equal 0, ("Ā" =~ re)
    assert_equal 0, ("Ő" =~ re)
    assert_equal 0, ("Ȁ" =~ re)
    assert_nil ("ÿ" =~ re)
    assert_nil ("ȁ" =~ re)
    assert_nil ("a" =~ re)
    # The negation reads the same class, so it draws the same boundary.
    neg = Regexp.new("[^" + re.source[1..-1])
    assert_nil ("Ő" =~ neg)
    assert_equal 0, ("ȁ" =~ neg)
  end
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

  # under /x whitespace is skipped between tokens, and a list is one token,
  # so the spaces that separate its codepoints stay
  assert_equal 0, (Regexp.new("\\u{61 62}", Regexp::EXTENDED) =~ "ab")

  # /i folds an ASCII letter reached through `\u`, like a literal one
  assert_equal 0, (/\u0061/i =~ "A")
end

assert("Regexp - \\u escapes in a character class") do
  skip unless __ENCODING__ == "UTF-8"
  assert_equal 0, (/[\u00b5]/ =~ "\xc2\xb5")
  assert_nil (/[\u00b5]/ =~ "u")
  assert_equal 0, (/[\u{3042}-\u{3044}]/ =~ "\xe3\x81\x83")
  assert_nil (/[\u{3042}-\u{3044}]/ =~ "\xe3\x81\x85")
  assert_equal 0, (/[a-\u{7a}]/ =~ "q")

  # every codepoint of a list is a member of its own, and the one next to a
  # '-' still opens or closes a range: the last before it, the first after it
  assert_equal ["a", "b"], "abc".scan(/[\u{61 62}]/)
  assert_equal ["a", "b", "c"], "abc-".scan(/[\u{61 62}-z]/)
  assert_equal ["a", "b", "c", "z"], "abcdz-".scan(/[a-\u{63 7a}]/)
  assert_equal ["c"], "abc".scan(/[^\u{61 62}]/)
end

assert("Regexp - a \\u list closing a character class range") do
  # The codepoint next to the '-' is the range end and the rest of the list
  # are members. The end used to be the last codepoint of the list, so the
  # class held a different set from CRuby's in either direction: `a-z` plus
  # `c` for /[a-\u{63 7a}]/, and `z` alone for /[a-\u{7a 41}]/, whose range
  # ran from `a` down to `A` and was dropped.
  assert_equal ["a", "b", "c", "z"], "abcdz".scan(/[a-\u{63 7a}]/)
  assert_equal ["a", "A", "z"], "aAzB".scan(/[a-\u{7a 41}]/)
  assert_equal ["a", "b", "c", "d"], "abcde".scan(/[\u{61 62}-\u{63 64}]/)
  # which is also the codepoint a reversed range is reported for
  assert_raise_with_message(RegexpError, "empty range in char class: /[b-\\u{61 63}]/") do
    Regexp.new("[b-\\u{61 63}]")
  end
  if __ENCODING__ == "UTF-8"
    assert_equal ["あ", "ぃ", "う"], "あぃぅう".scan(/[\u{3042}-\u{3044 3046}]/)
    assert_equal ["あ", "ぅ"], "あぃぅ".scan(/[\u{3044}-\u{3046 3042}]/)
    assert_raise(RegexpError) { Regexp.new("[\\u{3044}-\\u{3042 3046}]") }
  end
end

assert("Regexp - reversed character class range through \\u") do
  # `\u` can write a range backwards without the letters showing it. Such a
  # range holds nothing, and it used to be dropped without a word: a positive
  # class lacked the span and a negated one admitted everything.
  assert_raise_with_message(RegexpError, "empty range in char class: /[\\u{62}-\\u{61}]/") do
    Regexp.new("[\\u{62}-\\u{61}]")
  end
  assert_raise(RegexpError) { Regexp.new("[^\\u{62}-\\u{61}]") }
  # the last codepoint of a list opens the range, so it is the one compared
  assert_raise(RegexpError) { Regexp.new("[\\u{62 63}-a]") }
  assert_equal ["a", "b"], "abc".scan(/[\u{62 61}-a]/)
  # a range of one codepoint is not empty
  assert_equal ["a"], "abc".scan(/[\u{61}-\u{61}]/)
  assert_equal ["a"], "abc".scan(/[a-\u{61}]/)
  # A range between two characters above ASCII compares their codepoints. On
  # a build reading a String by byte the ends are the last bytes of their
  # spellings, whose order is not the codepoints' order.
  if __ENCODING__ == "UTF-8"
    assert_raise(RegexpError) { Regexp.new("[\\u{3044}-\\u{3042}]") }
    assert_raise(RegexpError) { Regexp.new("[\\u{100}-\\u{FF}]") }
    assert_equal 0, (/[\u{3042}-\u{3042}]/ =~ "あ")
  end
end

assert("Regexp - a \\u escape in a class names what spelling it out names") do
  # A class member is one character, and on a build whose characters are single
  # bytes a character above ASCII is not one: `[Ā]` holds the two bytes that
  # spell it, since read_class_atom() decodes one byte at a time there. Naming
  # that character with an escape has to come to the same members, or the two
  # halves of one pattern disagree about what the pattern holds.
  skip if __ENCODING__ == "UTF-8"
  spelled = Regexp.new("[\u{100}]")           # the character, written out
  named = Regexp.new("[\\u{100}]")            # the same character, named
  ["\u{100}", "\xC4".b, "\x80".b].each do |subject|
    assert_equal spelled.match?(subject), named.match?(subject)
  end
  # The character the escape names is found through the bytes it holds, which
  # is what the written out spelling answers too.
  assert_true named.match?("\u{100}")
  # An ASCII codepoint is a member of its own on either build, and a list still
  # gives every codepoint a membership with the one next to a '-' able to open
  # or close a range.
  assert_equal ["a", "b"], "abc".scan(/[\u{61 62}]/)
  assert_equal ["a", "b", "c"], "abc-".scan(/[\u{61 62}-z]/)
  assert_equal ["a", "b", "c", "z"], "abcdz-".scan(/[a-\u{63 7a}]/)
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
  # past the end of the pattern buffer. The subject that is the byte is
  # byte-indexed, since one read as UTF-8 spells no character at it, and the
  # engine walks the class there through a branch of its own.
  re = Regexp.new("[   \xff ]")
  assert_kind_of Regexp, re
  assert_nil (re =~ "x")
  assert_equal 0, (re =~ "\xff".b)
end

assert("Regexp - truncated UTF-8 at subject end") do
  # a lone multi-byte leader at the end of the subject must not read
  # past the end of the string buffer when matched against a class. The
  # subject is byte-indexed, since one read as UTF-8 spells no character at
  # the leader; there the leader is a byte of its own, and the walk that
  # reaches the end of the buffer is the one that has to stay inside it.
  assert_nil ("ab\xf0".b =~ /[cd]/)
  assert_equal 0, ("ab\xf0".b =~ /[^cd]+$/)
end

assert("Regexp - overlong UTF-8 is not the character it spells") do
  # C0 BC is the two-byte overlong spelling of "<" and E0 84 80 the three-byte
  # spelling of "Ā". A decoder that hands out a codepoint for these would let a
  # class hold a character the subject does not spell, so assert the class and
  # the literal together against the same subject. RFC 3629 puts these
  # sequences outside UTF-8, so a subject that carries one spells no character
  # for the decode to hand out and is byte-indexed below. That is where the
  # decode has to answer anyway: the class must hold no character the bytes do
  # not spell, whichever way the subject is indexed.
  # the pattern side decodes through the same helper, and its subject here is
  # whole UTF-8
  skip unless __ENCODING__ == "UTF-8"
  assert_false Regexp.new("[\xC0\xBC]").match?("<")
  # the shortest spelling on each side of the RFC 3629 bounds is still one
  # character
  assert_equal 1, "\u{0080}".scan(/./).size    # C2 80
  assert_equal 1, "\u{0800}".scan(/./).size    # E0 A0 80
  assert_equal 1, "\u{D7FF}".scan(/./).size    # ED 9F BF
  assert_equal 1, "\u{E000}".scan(/./).size    # EE 80 80
  assert_equal 1, "\u{10000}".scan(/./).size   # F0 90 80 80
  assert_equal 1, "\u{10FFFF}".scan(/./).size  # F4 8F BF BF
  assert_equal 0, ("\u{0800}" =~ Regexp.new("[\u{0800}]"))
  assert_equal 0, ("\u{10FFFF}" =~ Regexp.new("[\u{10FFFF}]"))
  # an overlong sequence holds no character on the subject side either
  assert_nil ("\xC0\xBC".b =~ /[<]/)
  assert_nil ("\xC0\xBC".b =~ /</)
  assert_equal 0, ("\xC0\xBC".b =~ /[^<]/)
  assert_equal "\xC0\xBC".b, "\xC0\xBC".b.gsub(/[<]/, "&lt;")
  assert_nil ("\xE0\x80\xBC".b =~ /[<]/)
  assert_false Regexp.new("[Ā]").match?("\xE0\x84\x80".b)
  assert_false (/Ā/.match?("\xE0\x84\x80".b))
  # surrogates and codepoints above U+10FFFF encode no character either, so
  # each byte stands on its own
  assert_equal 2, "\xC0\xBC".b.scan(/./).size
  assert_equal 3, "\xED\xA0\x80".b.scan(/./).size
  assert_equal 4, "\xF0\x80\x80\xBC".b.scan(/./).size
  assert_equal 4, "\xF4\x90\x80\x80".b.scan(/./).size
  assert_equal 4, "\xF5\x80\x80\x80".b.scan(/./).size
end

assert("Regexp - a pattern byte that starts no character is a byte in a class") do
  # A class used to read a lone continuation byte as the codepoint of its
  # number, so "[\xB5]" held U+00B5 while "\xB5" held the byte: one pattern
  # meant two things depending on which side of the brackets it was written.
  # CRuby settles it with the pattern's encoding and raises RegexpError for
  # either spelling; this gem has no encoding to consult, so it reads the byte
  # as the byte on both sides. Which of the two a class holds belongs to the
  # pattern, and a subject that is one such byte is byte-indexed below, since
  # one read as UTF-8 spells no character at it. There the byte is a byte on
  # the subject side too, which is what each pair below turns on: the byte
  # answers for itself, and the character that carries it answers for the
  # character.
  skip unless __ENCODING__ == "UTF-8"
  mu = "\xC2\xB5"  # U+00B5 MICRO SIGN, two bytes
  assert_nil (mu =~ Regexp.new("[\xB5]"))
  assert_nil (mu =~ Regexp.new("\xB5"))
  assert_equal mu.bytes, mu.gsub(Regexp.new("[\xB5]"), "!").bytes
  assert_equal mu.bytes, mu.gsub(Regexp.new("\xB5"), "!").bytes
  assert_equal 0, ("\xB5".b =~ Regexp.new("[\xB5]"))     # the byte alone is it
  assert_equal 1, (mu.b =~ Regexp.new("[\xB5]"))         # and so is one inside
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
  # Only a build that reads its patterns as UTF-8 can list that many members:
  # where a pattern is bytes there are 128 non-ASCII members in all, so the
  # loop below builds a handful of ranges and nothing can overflow.
  skip unless __ENCODING__ == "UTF-8"
  # append_as_bytes lays raw bytes into the string without moving how it is
  # read; a sum of Integer#chr pieces would come back read as bytes, and a
  # byte-read subject is answered by the byte on its own rather than the
  # character it begins.
  utf8 = ->(cp) {
    s = ""
    if cp < 0x800
      s.append_as_bytes(0xC0 | (cp >> 6))
      s.append_as_bytes(0x80 | (cp & 0x3F))
    else
      s.append_as_bytes(0xE0 | (cp >> 12))
      s.append_as_bytes(0x80 | ((cp >> 6) & 0x3F))
      s.append_as_bytes(0x80 | (cp & 0x3F))
    end
    s
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

assert("Regexp - a subject whose bytes are not UTF-8 is refused") do
  # CRuby raises ArgumentError for a subject holding a byte that stands for no
  # character, and mruby answered for it, so a program moved from one to the
  # other took a result CRuby would not have produced. `String#scrub` is how a
  # subject like this becomes matchable.
  skip unless __ENCODING__ == "UTF-8"
  broken = "あ\x80b"     # "あ" followed by a lone continuation byte

  assert_raise(ArgumentError) { broken =~ /b/ }
  assert_raise(ArgumentError) { /b/ =~ broken }
  assert_raise(ArgumentError) { /b/.match(broken) }
  assert_raise(ArgumentError) { /b/.match?(broken) }
  assert_raise(ArgumentError) { /b/ === broken }
  assert_raise(ArgumentError) { broken.match(/b/) }
  assert_raise(ArgumentError) { broken.match?(/b/) }
  assert_raise(ArgumentError) { broken.index(/b/) }
  assert_raise(ArgumentError) { broken.rindex(/b/) }
  assert_raise(ArgumentError) { broken.byteindex(/b/) }
  assert_raise(ArgumentError) { broken.byterindex(/b/) }
  assert_raise(ArgumentError) { broken[/b/] }
  assert_raise(ArgumentError) { broken.sub(/b/, "!") }
  assert_raise(ArgumentError) { broken.sub(/b/) { "!" } }
  assert_raise(ArgumentError) { broken.gsub(/b/, "!") }
  assert_raise(ArgumentError) { broken.gsub(/b/) { "!" } }
  assert_raise(ArgumentError) { broken.scan(/b/) }
  assert_raise(ArgumentError) { broken.split(/b/) }
  assert_raise(ArgumentError) { broken.partition(/b/) }
  assert_raise(ArgumentError) { broken.rpartition(/b/) }
  assert_raise(ArgumentError) { broken.start_with?(/b/) }
  assert_raise(ArgumentError) { broken.dup.sub!(/b/, "!") }
  assert_raise(ArgumentError) { broken.dup.gsub!(/b/, "!") }
  assert_raise(ArgumentError) { broken.dup.slice!(/b/) }
  assert_raise(ArgumentError) { broken.dup[/b/] = "!" }

  # A String pattern is a literal, which CRuby searches for byte by byte
  # without reading the subject as UTF-8, so these answer there and here.
  # `scan` is the exception CRuby itself makes: it refuses a literal too.
  assert_equal "あ\x80!", broken.sub("b", "!")
  assert_equal "あ\x80!", broken.gsub("b", "!")
  assert_equal "あ\x80!", broken.sub("b") { "!" }
  assert_equal "あ\x80!", broken.gsub("b") { "!" }
  bang = broken.dup
  bang.sub!("b", "!")
  assert_equal "あ\x80!", bang
  bang = broken.dup
  bang.gsub!("b", "!")
  assert_equal "あ\x80!", bang
  # The position it publishes is the one the string's own indexing answers.
  broken.sub("b", "!")
  assert_equal broken.index("b"), $~.begin(0)
  assert_raise(ArgumentError) { broken.scan("b") }
  assert_raise(ArgumentError) { broken.scan("b") {} }

  # `split` is the other exception, and it takes every pattern with it: CRuby
  # refuses a String, a nil and the awk form as well as a Regexp. A String
  # pattern reaches core's `split` here, which searches for a literal without
  # this gem in the way, so the refusal is asked for at the entry instead.
  assert_raise(ArgumentError) { broken.split("b") }
  assert_raise(ArgumentError) { broken.split }
  assert_raise(ArgumentError) { broken.split(" ") }
  assert_raise(ArgumentError) { broken.split("b", -1) }
  assert_raise(ArgumentError) { broken.split("b", 2) }
  # A limit of 1 hands the subject back whole without reading it, whatever the
  # pattern, and CRuby answers for that too.
  assert_equal [broken], broken.split("b", 1)
  assert_equal [broken], broken.split(nil, 1)
  assert_equal [broken], broken.split(" ", 1)
  assert_equal [broken], broken.split(/b/, 1)
  # The limit is converted before the subject is read, as in CRuby.
  assert_raise(TypeError) { broken.split("b", "x") }

  # A byte-indexed subject is indexed by byte throughout, so its bytes make no
  # claim that could be broken and it goes through as it always did.
  assert_equal 4, (broken.b =~ /b/)
  assert_equal 4, broken.b.match(/b/).begin(0)
  assert_equal "あ\x80!".b, broken.b.sub(/b/, "!")
  assert_equal ["あ\x80".b], broken.b.split("b")

  # A whole subject is untouched, including one the walk reads to the end.
  assert_equal 2, ("あいb" =~ /b/)
  assert_equal 1, ("a\u{10FFFF}b" =~ /b\z|\u{10FFFF}/)
end

assert("Regexp - a broken subject is refused before the pattern is read") do
  # CRuby reads the subject before it looks at the pattern, so a Regexp that
  # never compiled is told about the subject first, and here too.
  skip unless __ENCODING__ == "UTF-8"
  broken = "あ\x80b"
  re = Regexp.allocate
  msg = "invalid byte sequence in UTF-8"

  assert_raise_with_message(ArgumentError, msg) { re =~ broken }
  assert_raise_with_message(ArgumentError, msg) { re.match(broken) }
  assert_raise_with_message(ArgumentError, msg) { re.match?(broken) }
  assert_raise_with_message(ArgumentError, msg) { re === broken }
  assert_raise_with_message(ArgumentError, msg) { broken.match?(re) }
  assert_raise_with_message(ArgumentError, msg) { broken.index(re) }
  assert_raise_with_message(ArgumentError, msg) { broken.rindex(re) }
  assert_raise_with_message(ArgumentError, msg) { broken.byteindex(re) }
  assert_raise_with_message(ArgumentError, msg) { broken.byterindex(re) }
  assert_raise_with_message(ArgumentError, msg) { broken.sub(re, "!") }
  assert_raise_with_message(ArgumentError, msg) { broken.sub(re) { "!" } }
  assert_raise_with_message(ArgumentError, msg) { broken.gsub(re, "!") }
  assert_raise_with_message(ArgumentError, msg) { broken.gsub(re) { "!" } }
  assert_raise_with_message(ArgumentError, msg) { broken.scan(re) }
  assert_raise_with_message(ArgumentError, msg) { broken.scan(re) {} }

  # A sound subject leaves the pattern to be refused.
  assert_raise(TypeError) { re =~ "b" }
  assert_raise(TypeError) { re === "b" }
  assert_raise(TypeError) { "b".gsub(re) { "!" } }

  # `split` is the one search where CRuby reads the pattern first, to see
  # whether it can be split on as a literal, and the order is kept here. A
  # limit of 1 answers before either is read, there and here.
  assert_raise(TypeError) { broken.split(re) }
  assert_equal [broken], broken.split(re, 1)
end

assert("Regexp - a piece of a byte-read subject is byte-read") do
  # What a match hands back is bytes of the subject, read the way the subject
  # was. Encoding introspection lives in mruby-encoding, which this gem does
  # not depend on, so ask only where it is present.
  skip unless "".respond_to?(:encoding)
  skip unless __ENCODING__ == "UTF-8"
  subject = "a\x80b".b
  assert_equal Encoding::BINARY, subject.match(/a/)[0].encoding
  subject =~ /a/
  assert_equal Encoding::BINARY, $~[0].encoding
  assert_equal Encoding::BINARY, $&.encoding
  assert_equal Encoding::BINARY, subject.scan(/./)[0].encoding
  # a piece of a subject read as UTF-8 goes on reading as UTF-8
  assert_equal Encoding::UTF_8, "あb".match(/b/)[0].encoding
end

assert("Regexp - what sub and gsub build out of a byte-read subject") do
  # The result is the subject's bytes with the replacement spliced in, so it
  # is read the way the subject was, and a replacement of byte-read bytes
  # above ASCII marks a plain subject's result the way any appended byte-read
  # bytes do.
  skip unless "".respond_to?(:encoding)
  skip unless __ENCODING__ == "UTF-8"
  subject = "a\x80b".b
  assert_equal Encoding::BINARY, subject.sub(/a/, "-").encoding
  assert_equal Encoding::BINARY, subject.gsub(/a/, "-").encoding
  assert_true subject.gsub(/a/, "-").valid_encoding?
  assert_equal Encoding::BINARY, subject.gsub(/a/) { "-" }.encoding
  assert_equal Encoding::BINARY, "ab".gsub(/a/, 171.chr).encoding
  assert_equal Encoding::BINARY, "ab".sub(/a/, 171.chr).encoding
  # a replacement of ASCII bytes moves nothing
  assert_equal Encoding::UTF_8, "ab".gsub(/a/, "-".b).encoding
  # and neither does one a search that matched nothing never spliced in
  assert_equal Encoding::UTF_8, "ab".gsub(/x/, 171.chr).encoding
  assert_equal Encoding::UTF_8, "ab".gsub("x", 171.chr).encoding
  assert_equal Encoding::UTF_8, "ab".sub(/x/, 171.chr).encoding
  # a byte-read subject is read as bytes whether anything was spliced or not
  assert_equal Encoding::BINARY, subject.gsub(/x/, "-").encoding
end

assert("Regexp - the match a gsub block leaves behind reads as the receiver does") do
  # The search that ends the block form of `gsub` runs on the receiver as
  # the block left it, and it is spared where the receiver still reads as it
  # did when the last match was made. A block that changed how the receiver
  # is read without changing a byte has changed what a search reads of it:
  # `s.replace(s.b)` keeps every byte and makes them byte-read, so the match
  # left behind counts its offsets in bytes, where the match the loop had
  # counted characters. Bytes alone would take that receiver for unchanged.
  skip unless __ENCODING__ == "UTF-8"
  s = "héllo"
  s.gsub(/l/) { s.replace(s.b); "L" }
  assert_equal 4, $~.begin(0)
  assert_equal 6, $~.string.size
  s = "héllo"
  s.gsub(/l/) { "L" }
  assert_equal 3, $~.begin(0)
  assert_equal 5, $~.string.size

  # It takes a multibyte character *before* the match for the two readings to
  # answer differently: with the match at "l" of "héllo" a republish and a
  # fresh search name the same offset, so that pair cannot tell them apart.
  # Here the match is at "i" with two three-byte characters in front, so
  # reading the receiver as bytes moves it from character 1 to byte 3.
  s = "あiう"
  s.gsub(/i/) { s.replace(s.b); "X" }
  assert_equal 3, $~.begin(0)
  assert_equal 7, $~.string.size
end

assert('Regexp - a word boundary sits beside any script') do
  # `\b` reads the word characters `[[:word:]]` holds rather than the ASCII
  # set `\w` names. The two constructs are not two answers to one question: a
  # class can be written another way, and a boundary cannot, so the shorthand
  # keeps CRuby's ASCII set and the boundary reads every script, as CRuby's
  # does. Without a class it took one byte, and no byte of a multi-byte
  # character is a word character, so /\b/ found none in "ααα".
  #
  # The boundary reads the bracket's set, so having one above ASCII takes a
  # build that classifies there. `"あ".length == 1` does not say that: a
  # build reading its strings as UTF-8 without the ctype table answers 1 and
  # still holds no word character above ASCII. Ask the bracket itself.
  skip unless "あ".length == 1
  skip "this build classifies only ASCII" unless "α" =~ /[[:word:]]/

  assert_equal 0, ("ααα" =~ /\A\b/)
  assert_equal 0, ("漢字" =~ /\A\b/)
  assert_equal "ααα", "ααα"[/\A[[:word:]]+\z/]
  assert_nil ("ααα" =~ /\A\B/)

  # a boundary sits at each end of a run, whatever the widths in it are
  assert_equal ["漢字とKanji", "abc"], "漢字とKanji abc".scan(/\b[[:word:]]+\b/)
  assert_equal ["a", "α", "b"], "a α b".scan(/\b[[:word:]]+\b/)
  # 1, 2, 3 and 4-byte characters in one subject
  assert_equal ["a", "α", "漢", "𠮷", "b"], "a α 漢 𠮷 b".scan(/\b[[:word:]]+\b/)

  # `\w` is untouched: still the ASCII set, as in CRuby
  assert_nil ("ααα" =~ /\w/)
  assert_equal "a", "a α"[/\w/]

  # and a character that is in neither is still no word character
  assert_nil ("・" =~ /\A\b/)
  assert_equal 0, ("・" =~ /\A\B/)

  # A binary subject holds bytes rather than characters, so a byte at or above
  # 0x80 stands for no character and the table must not be asked about it:
  # 0xB5 alone is that byte, not the word character it spells in UTF-8.
  if "".respond_to?(:force_encoding)
    byte = "\xB5".force_encoding("ASCII-8BIT")
    assert_nil (byte =~ /\A\b/)
    assert_equal 0, (byte =~ /\A\B/)
  end
end

assert("Regexp - an absent repeater's run stops on a character boundary") do
  need_backtracking_stack
  skip unless __ENCODING__ == "UTF-8"

  # The run may not hold the body's match, so it stops before the first byte
  # of the character the match begins at, not one byte back from its end.
  assert_equal "い", /(?~あ)/.match("いあう")[0]
  assert_equal "", /(?~い)/.match("いあう")[0]
  assert_equal "いあ", /(?~う)/.match("いあう")[0]
  assert_equal "いあう", /(?~x)/.match("いあう")[0]
  # and it gives the characters back one at a time, not the bytes
  assert_equal "いあう", /(?~x)う/.match("いあう")[0]

  # A binary subject is bytes, so the same pattern stops one byte into the
  # character the match begins at.
  if "".respond_to?(:force_encoding)
    bin = "いあう".dup.force_encoding("ASCII-8BIT")
    pat = Regexp.new("(?~\xE3\x81\x82".dup.force_encoding("ASCII-8BIT") + ")")
    assert_equal 5, pat.match(bin)[0].bytesize
  end
end
