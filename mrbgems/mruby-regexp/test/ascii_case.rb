# Only compiled into mrbtest when the build does NOT define
# MRB_REGEXP_UNICODE_CASE; see the gem's mrbgem.rake. With the option every
# pattern refused here compiles and matches instead.
assert("Regexp - /i refuses what ASCII folding cannot answer") do
  # Folding ASCII and carrying on would answer wrongly rather than narrowly:
  # the missing fold is a missed match in the plain and class forms, and the
  # same gap with the sign flipped is a false accept in the negated class.
  assert_raise(RegexpError) { Regexp.new("Ā", Regexp::IGNORECASE) }
  assert_raise(RegexpError) { Regexp.new("[Ā]", Regexp::IGNORECASE) }
  assert_raise(RegexpError) { Regexp.new("[^Ā]", Regexp::IGNORECASE) }
  assert_raise(RegexpError) { Regexp.new("[Ā-Ă]", Regexp::IGNORECASE) }
  assert_raise(RegexpError) { Regexp.new("Σ", Regexp::IGNORECASE) }
  assert_raise(RegexpError) { Regexp.new("д", Regexp::IGNORECASE) }
  assert_raise(RegexpError) { Regexp.new("aĀb", Regexp::IGNORECASE) }
  # A backslash before a multibyte character has no escape meaning, so the
  # escaped spelling is refused exactly as the plain one is.
  assert_raise(RegexpError) { Regexp.new("\\Ā", Regexp::IGNORECASE) }
  # A range straddling the ASCII boundary is refused for its non-ASCII half,
  # which the split leaves in the codepoint list; the ASCII half it could have
  # answered on its own does not save it.
  assert_raise(RegexpError) { Regexp.new("[a-Ā]", Regexp::IGNORECASE) }
  # A source the option build cannot fold either is refused all the same: the
  # build has no data to tell it apart from one that would have folded.
  assert_raise(RegexpError) { Regexp.new("ß", Regexp::IGNORECASE) }
  assert_raise(RegexpError) { Regexp.new("ﬀ", Regexp::IGNORECASE) }
  # A `\u` escape names a character rather than spelling it out, and naming one
  # does not make it a character this build can fold. Both spellings of the
  # escape reach the refusal, in the literal path and in the class path alike.
  assert_raise(RegexpError) { Regexp.new("\\u{100}", Regexp::IGNORECASE) }
  assert_raise(RegexpError) { Regexp.new("\\u0100", Regexp::IGNORECASE) }
  assert_raise(RegexpError) { Regexp.new("[\\u{100}]", Regexp::IGNORECASE) }
  assert_raise(RegexpError) { Regexp.new("[^\\u{100}]", Regexp::IGNORECASE) }
  assert_raise(RegexpError) { Regexp.new("[\\u{100}-\\u{102}]", Regexp::IGNORECASE) }
  # The message names the option, so hitting this says what to do about it.
  begin
    Regexp.new("Ā", Regexp::IGNORECASE)
  rescue RegexpError => e
    assert_true e.message.include?("MRB_REGEXP_UNICODE_CASE")
  end
end

assert("Regexp - /i leaves alone what it can answer") do
  # The test is whether a codepoint has a case folding, not whether it is
  # non-ASCII. A script without case has nothing to fold, so these compile and
  # match exactly as they always have.
  assert_true(/日本/i.match?("日本"))
  assert_true(/です/i.match?("です"))
  assert_true(/العربية/i.match?("العربية"))
  assert_true(/😀/i.match?("😀"))
  assert_true(/[日本]+/i.match?("本日"))
  assert_false(/[^日]/i.match?("日"))
  # Without /i there is nothing to refuse either.
  assert_true(/Ā/.match?("Ā"))
  assert_false(/Ā/.match?("ā"))
  assert_true(/[^Ā]/.match?("ā"))
  # U+212A folds to ASCII 'k', which this build has without the table, so the
  # `\u` spelling of it compiles and reaches both cases of the letter.
  assert_true(/\u{212a}/i.match?("k"))
  assert_true(/\u{212a}/i.match?("K"))
  assert_true(/[\u{212a}]/i.match?("k"))
  assert_false(/[^\u{212a}]/i.match?("K"))
end
