# Only compiled into mrbtest when the build classifies characters by Unicode,
# which is where a build reading its strings as characters stands unless it
# says otherwise; see the gem's mrbgem.rake. Classifying by ASCII, every
# bracket below holds ASCII and nothing above it; ascii_ctype.rb asserts that.
assert("Regexp - POSIX brackets classify by Unicode above ASCII") do
  # Every character here lies above ASCII, so it is a character to classify
  # only where the pattern and the subject are read as characters. A build
  # that reads its strings by byte has the table and nothing to spend it on.
  skip unless __ENCODING__ == "UTF-8"
  # Each bracket, members and non-members above ASCII. The members are picked
  # to be what the type is and not merely what its name suggests: [:upper:]
  # holds a Roman numeral and a circled letter and [:lower:] a feminine
  # ordinal, since Uppercase and Lowercase are the properties and not the
  # letter categories, [:alpha:] holds the combining ypogegrammeni U+0345 the
  # property counts as a letter, [:word:] the zero width joiner, and [:cntrl:]
  # both ends of the C1 range it is above ASCII. What is spelled as an escape
  # is a character that would not show on the page: a space, a control, a
  # format character, a mark or an unassigned codepoint.
  members = {
    "alpha" => ["あ", "Ā", "ª", "\u{345}", "Ⅷ", "Ⓐ"],
    "digit" => ["１", "𝟎"],
    "alnum" => ["あ", "１"],
    "upper" => ["Ā", "Ⅷ", "Ⓐ"],
    "lower" => ["ā", "ª", "ʰ", "\u{345}"],
    "space" => ["\u{A0}", "\u{2028}", "\u{85}", "\u{1680}"],
    "blank" => ["\u{A0}", "\u{1680}"],
    "word"  => ["あ", "\u{300}", "‿", "１", "\u{200D}"],
    "cntrl" => ["\u{80}", "\u{85}", "\u{9F}"],
    "punct" => ["‿", "「", "«"],
    "graph" => ["あ", "€", "\u{200B}", "\u{AD}", "\u{E000}"],
    "print" => ["あ", "\u{A0}", "\u{200B}", "\u{E000}"],
  }
  others = {
    "alpha" => ["１", "\u{300}", "‿", "€", "\u{A0}"],
    "digit" => ["²", "あ", "Ⅷ"],
    "alnum" => ["²", "\u{300}", "€"],
    "upper" => ["ā", "ǅ", "あ", "ª"],
    "lower" => ["Ā", "ǅ", "あ", "Ⅷ"],
    "space" => ["\u{200B}", "\u{AD}", "あ"],
    "blank" => ["\u{2028}", "\u{85}", "あ"],
    "word"  => ["²", "€", "「", "\u{A0}", "\u{200B}"],
    "cntrl" => ["\u{A0}", "\u{200B}", "あ"],
    "punct" => ["€", "あ", "\u{200B}"],
    "graph" => ["\u{A0}", "\u{2028}", "\u{85}", "\u{378}", "\u{10FFFF}"],
    "print" => ["\u{2028}", "\u{85}", "\u{378}", "\u{10FFFF}"],
  }
  members.each do |name, chars|
    yes = Regexp.new("[[:#{name}:]]")
    not_yes = Regexp.new("[[:^#{name}:]]")
    outside = Regexp.new("[^[:#{name}:]]")
    chars.each do |ch|
      assert_true yes.match?(ch), "[:#{name}:] holds U+#{ch.ord.to_s(16)}"
      assert_false not_yes.match?(ch), "[:^#{name}:] does not hold U+#{ch.ord.to_s(16)}"
      assert_false outside.match?(ch), "[^[:#{name}:]] does not hold U+#{ch.ord.to_s(16)}"
    end
    others[name].each do |ch|
      assert_false yes.match?(ch), "[:#{name}:] does not hold U+#{ch.ord.to_s(16)}"
      assert_true not_yes.match?(ch), "[:^#{name}:] holds U+#{ch.ord.to_s(16)}"
      assert_true outside.match?(ch), "[^[:#{name}:]] holds U+#{ch.ord.to_s(16)}"
    end
  end
  # A run of characters, and the same run under a negated bracket, since a
  # bracket that admits a character has to consume the whole of it.
  assert_equal "あいう", "123あいう456".match(/[[:alpha:]]+/)[0]
  assert_equal "あいう", "123あいう456".match(/[[:^digit:]]+/)[0]
  assert_equal "１２３", "abc１２３def".match(/[[:digit:]]+/)[0]
  assert_equal ["あ", "い"], "あ\u{3000}い".scan(/[[:^space:]]/)
  # A lookbehind steps back over the whole of the character a bracket admits.
  assert_equal 1, "あx" =~ /(?<=[[:alpha:]])x/
  assert_nil "あx" =~ /(?<![[:alpha:]])x/
  # [:xdigit:] and [:ascii:] are sets ASCII defines, so they hold nothing
  # above it on any build and their negations hold everything.
  assert_false "Ａ".match?(/[[:xdigit:]]/)
  assert_true "Ａ".match?(/[[:^xdigit:]]/)
  assert_false "あ".match?(/[[:ascii:]]/)
  assert_true "あ".match?(/[[:^ascii:]]/)
end

assert("Regexp - POSIX brackets combine above ASCII") do
  skip unless __ENCODING__ == "UTF-8"
  # Two brackets in one class hold the union.
  assert_equal "あ１", "あ１€".match(/[[:alpha:][:digit:]]+/)[0]
  # Two negated brackets hold every character that lacks either type, which
  # here is every character: あ is no digit and １ no letter.
  assert_equal "あ１€", "あ１€".match(/[[:^alpha:][:^digit:]]+/)[0]
  # Ā is a letter and upper case, so it lacks neither and is left out; ā is a
  # letter alone.
  assert_false "Ā".match?(/[[:^alpha:][:^upper:]]/)
  assert_true "ā".match?(/[[:^alpha:][:^upper:]]/)
  assert_true "€".match?(/[[:^alpha:][:^upper:]]/)
  # A bracket beside a written member or range.
  assert_equal "あ€", "あ€!".match(/[€[:alpha:]]+/)[0]
  assert_equal "あĀ", "あĀ!".match(/[Ā-ā[:alpha:]]+/)[0]
  # And the negated class over all of it.
  assert_equal "!", "あ€!".match(/[^€[:alpha:]]/)[0]
end

assert("Regexp - POSIX brackets fold above ASCII under /i") do
  skip unless __ENCODING__ == "UTF-8"
  upper = Regexp.new("[[:upper:]]", Regexp::IGNORECASE)
  lower = Regexp.new("[[:lower:]]", Regexp::IGNORECASE)
  # A bracket under /i holds a character whenever it holds one sharing its
  # folding: [:upper:] reaches ā through Ā, and [:lower:] Ā through ā.
  assert_true upper.match?("ā")
  assert_true lower.match?("Ā")
  assert_true upper.match?("ⅷ")
  assert_true lower.match?("Ⅷ")
  # A title case letter is neither, and folds with a letter that is each:
  # ǅ reaches [:upper:] through Ǆ and [:lower:] through ǆ.
  assert_false Regexp.new("[[:upper:]]").match?("ǅ")
  assert_false Regexp.new("[[:lower:]]").match?("ǅ")
  assert_true upper.match?("ǅ")
  assert_true lower.match?("ǅ")
  # A folding that reaches ASCII: U+017F folds with 's', so [:upper:] under
  # /i holds it through 'S', and U+212A with 'k'.
  assert_true upper.match?("ſ")
  assert_true lower.match?("K")
  # A negated bracket folds the same way, so [:^upper:] under /i holds Ā
  # through ā, and both letters of an ASCII pair.
  not_upper = Regexp.new("[[:^upper:]]", Regexp::IGNORECASE)
  assert_true not_upper.match?("Ā")
  assert_true not_upper.match?("A")
  assert_true not_upper.match?("K")
  # The negated class is the closed class turned around, so [^[:upper:]]
  # under /i rejects what [:upper:] reached.
  assert_false Regexp.new("[^[:upper:]]", Regexp::IGNORECASE).match?("ā")
  assert_false Regexp.new("[^[:lower:]]", Regexp::IGNORECASE).match?("Ā")
  # A type without case does not grow under /i.
  assert_false Regexp.new("[[:punct:]]", Regexp::IGNORECASE).match?("ſ")
  assert_false Regexp.new("[[:digit:]]", Regexp::IGNORECASE).match?("Ā")
  # Nor does one that holds both cases already.
  assert_true Regexp.new("[[:alpha:]]", Regexp::IGNORECASE).match?("ẞ")
  assert_false Regexp.new("[[:^alpha:]]", Regexp::IGNORECASE).match?("K")
end

assert("Regexp - POSIX brackets read a byte above 127 as no character") do
  # A byte-indexed subject hands out bytes, and a byte has no type: it is in
  # no positive bracket and in every negated one, whatever it would spell as
  # part of a character. This is what CRuby answers for an ASCII-8BIT subject.
  bytes = "Ā".b
  assert_nil bytes =~ /[[:alpha:]]/
  assert_nil bytes =~ /[[:print:]]/
  assert_equal 0, bytes =~ /[[:^alpha:]]/
  assert_equal 0, bytes =~ /[[:^print:]]/
  assert_equal 0, bytes =~ /[^[:alpha:]]/
end
