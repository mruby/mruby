# Only compiled into mrbtest when the build converts case by Unicode, which is
# where a build reading its strings as characters stands unless it says
# otherwise; see the gem's mrbgem.rake. Converting by ASCII, every pattern
# below would be refused instead.
assert("Regexp - Unicode case folding under /i") do
  # Every source and counterpart here lies above ASCII, so they are characters
  # to fold only where the pattern and the subject are read as characters. A
  # build that reads its strings by byte has the table and nothing to spend it
  # on: the bytes below spell no character it can fold.
  skip unless __ENCODING__ == "UTF-8"
  # A literal and its counterpart, in both directions.
  assert_equal "ā", "ā".match(/Ā/i)[0]
  assert_equal "Ā", "Ā".match(/ā/i)[0]
  assert_equal "σ", "σ".match(/Σ/i)[0]
  assert_equal "д", "д".match(/Д/i)[0]
  # A backslash before a multibyte character has no escape meaning, so the
  # escaped spelling of a literal folds the same way the plain one does, and
  # the whole character stays one atom for a quantifier to bind to.
  assert_equal "ā", "ā".match(/\Ā/i)[0]
  assert_equal "āā", "āā".match(/\Ā{2}/i)[0]
  # Inside a class, and its negation.
  assert_equal "ā", "ā".match(/[Ā]/i)[0]
  assert_nil "ā".match(/[^Ā]/i)
  # Three codepoints share one fold: U+03A3 and U+03C2 both fold to U+03C3.
  # A class written with any of them has to reach the other two, which takes
  # the fold they share as a stepping stone rather than one hop from what was
  # written.
  assert_equal "ς", "ς".match(/[Σ]/i)[0]
  assert_equal "Σ", "Σ".match(/[ς]/i)[0]
  assert_nil "ς".match(/[^Σ]/i)
  # U+0103 folds from U+0102, which the range holds, so /i reaches it even
  # though it sits past the upper bound itself.
  assert_equal "ă", "ă".match(/[Ā-Ă]/i)[0]
  assert_nil "ą".match(/[Ā-Ă]/i)
  # A range straddling the ASCII boundary folds on both sides of the split.
  assert_equal "A", "A".match(/[a-Ā]/i)[0]
  assert_equal "à", "à".match(/[a-Ā]/i)[0]
  assert_equal "ā", "ā".match(/[a-Ā]/i)[0]
  # A counterpart of a different byte length: U+212A folds to 'k' and U+017F
  # to 's', so the two sides of the choice are 3 and 1, and 2 and 1, bytes.
  assert_equal "K", "K".match(/k/i)[0]
  assert_equal "k", "k".match(/K/i)[0]
  assert_equal "ſ", "ſ".match(/s/i)[0]
  # A `\u` escape names the character another pattern would spell out, so /i
  # folds it the same way either spelling is folded. That holds for both
  # spellings of the escape, and inside a class as well as outside one.
  assert_equal "ā", "ā".match(/\u{100}/i)[0]
  assert_equal "Ā", "Ā".match(/\u{101}/i)[0]
  assert_equal "ā", "ā".match(/\u0100/i)[0]
  assert_equal "ā", "ā".match(/[\u{100}]/i)[0]
  assert_nil "ā".match(/[^\u{100}]/i)
  # A range written with escapes closes under folding as the spelled one does:
  # U+0103 folds from U+0102, which the range holds.
  assert_equal "ă", "ă".match(/[\u{100}-\u{102}]/i)[0]
  assert_nil "ą".match(/[\u{100}-\u{102}]/i)
  # Backreferences compare folded too.
  assert_equal "Āā", "Āā".match(/(Ā)\1/i)[0]
  # Without /i nothing folds.
  assert_nil "ā".match(/Ā/)
  assert_nil "K".match(/k/)
  # A fold of several codepoints can still leave a single counterpart, and
  # that much is applied: U+1E9E lower cases to U+00DF and the two fold alike.
  assert_equal "ẞ", "ẞ".match(/ß/i)[0]
  assert_nil "ẞ".match(/[^ß]/i)
  # The expansion into several codepoints is what stays out of reach.
  assert_nil "ss".match(/ß/i)
  assert_nil "ff".match(/ﬀ/i)
end

assert("Regexp - /i literals share one class per codepoint") do
  skip unless __ENCODING__ == "UTF-8"
  # A folded literal compiles to a class, and a class id is a byte, so a
  # pattern holds at most 256 of them. Every occurrence used to take one, and a
  # phrase of a few hundred letters was refused as too many character
  # classes; the second occurrence of a codepoint now names the class the
  # first one made.
  re = Regexp.new("д" * 300, Regexp::IGNORECASE)
  assert_true re.match?("Д" * 300)
  assert_true re.match?("д" * 300)
  assert_false re.match?("Д" * 299)
  # Both cases of a letter, and a run of one letter next to a run of another,
  # each cost as many classes as they have distinct codepoints.
  re = Regexp.new("дД" * 150, Regexp::IGNORECASE)
  assert_true re.match?("ДД" * 150)
  assert_true re.match?("дд" * 150)
  re = Regexp.new("д" * 256 + "a" * 256, Regexp::IGNORECASE)
  assert_true re.match?("Д" * 256 + "A" * 256)
  # A `\u` escape and a spelled out character name the same codepoint, so
  # they share the class as well.
  re = Regexp.new("\\u{434}" * 150 + "д" * 150, Regexp::IGNORECASE)
  assert_true re.match?("Д" * 300)
  # The class is consulted only where /i is on: outside it the same codepoint
  # is its bytes and matches its own case alone.
  re = Regexp.new("(?i:д)д")
  assert_true re.match?("Дд")
  assert_false re.match?("ДД")
  # What the cap counts now is distinct codepoints. Cyrillic, Latin-1, Greek
  # and Armenian letters below come to 281 of them, past what the id can hold,
  # so the pattern is still refused; CRuby has no such cap.
  cyr = (0x400..0x45f).map {|c| c.chr("UTF-8") }.join
  lat = ((0xc0..0xd6).to_a + (0xd8..0xde).to_a + (0xe0..0xf6).to_a +
         (0xf8..0xfe).to_a).map {|c| c.chr("UTF-8") }.join
  grk = ((0x391..0x3a1).to_a + (0x3a3..0x3a9).to_a +
         (0x3b1..0x3c9).to_a).map {|c| c.chr("UTF-8") }.join
  arm = ((0x531..0x556).to_a + (0x561..0x586).to_a).map {|c| c.chr("UTF-8") }.join
  assert_equal 281, (cyr + lat + grk + arm).length
  re = Regexp.new(cyr + lat + grk, Regexp::IGNORECASE)
  assert_true re.match?((cyr + lat + grk).upcase)
  assert_raise(RegexpError) { Regexp.new(cyr + lat + grk + arm, Regexp::IGNORECASE) }
end
