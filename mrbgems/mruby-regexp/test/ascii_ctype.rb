# Only compiled into mrbtest when the build classifies characters by ASCII,
# whether by MRB_USE_ASCII_CTYPE or by reading its strings as bytes; see the
# gem's mrbgem.rake. Where it classifies by Unicode, every bracket below holds
# the character it is asked about, and unicode_ctype.rb asserts that.
assert("Regexp - POSIX brackets hold ASCII and nothing above it") do
  # A bracket holds the ASCII of its type and no character above ASCII, so its
  # negation holds every one: this build has no table to say what a character
  # above ASCII is, and a build reading its strings by byte has no character
  # to ask about. The same rows in unicode_ctype.rb answer the other way.
  %w[alpha digit alnum upper lower space blank word cntrl print graph punct].each do |name|
    yes = Regexp.new("[[:#{name}:]]")
    not_yes = Regexp.new("[[:^#{name}:]]")
    outside = Regexp.new("[^[:#{name}:]]")
    ["あ", "Ā", "１", "\u{A0}", "\u{85}"].each do |ch|
      assert_false yes.match?(ch), "[:#{name}:] does not hold U+#{ch.ord.to_s(16)}"
      assert_true not_yes.match?(ch), "[:^#{name}:] holds U+#{ch.ord.to_s(16)}"
      assert_true outside.match?(ch), "[^[:#{name}:]] holds U+#{ch.ord.to_s(16)}"
    end
  end
  # The ASCII of each is what every build holds.
  assert_equal "abc", "あabc".match(/[[:alpha:]]+/)[0]
  assert_equal "12", "１12".match(/[[:digit:]]+/)[0]
  # /i folds nothing across the boundary that the table would have supplied:
  # [:upper:] under /i is [A-Za-z] and the two sources that fold to ASCII.
  assert_false Regexp.new("[[:upper:]]", Regexp::IGNORECASE).match?("ā")
  assert_true Regexp.new("[[:^upper:]]", Regexp::IGNORECASE).match?("ā")
end
