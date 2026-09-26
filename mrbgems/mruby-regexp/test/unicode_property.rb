# Only compiled into mrbtest when the build classifies characters by Unicode;
# see the gem's mrbgem.rake. Elsewhere the general categories and the emoji
# properties are refused, and ascii_ctype.rb asserts that.

assert("Regexp - \\p reads the general categories") do
  skip unless __ENCODING__ == "UTF-8"
  s = "Aa1 _-€é́あ🔔"
  assert_equal ["A"], s.scan(/\p{Lu}/)
  assert_equal "aé", s.scan(/\p{Ll}/).join
  assert_equal "Aaéあ", s.scan(/\p{L}/).join
  assert_equal ["1"], s.scan(/\p{Nd}/)
  assert_equal ["_", "-"], s.scan(/\p{Pc}/) + s.scan(/\p{Pd}/)
  assert_equal ["_", "-"], s.scan(/\p{P}/)
  assert_equal ["€"], s.scan(/\p{Sc}/)
  assert_equal [" "], s.scan(/\p{Zs}/)
  assert_equal 1, s.scan(/\p{Mn}/).length

  # One of each of the rest, and a codepoint of each that is not one.
  {
    "Lt" => "ǅ", "Lm" => "ʰ", "Lo" => "あ", "Mc" => "\u0903", "Me" => "\u20dd",
    "Nl" => "Ⅷ", "No" => "²", "Ps" => "(", "Pe" => ")", "Pi" => "«", "Pf" => "»",
    "Po" => "!", "Sm" => "+", "Sk" => "^", "So" => "©", "Zl" => "\u2028",
    "Zp" => "\u2029", "Cc" => "\t", "Cf" => "\u00ad", "Co" => "\ue000",
    "Cn" => "\u0378", "M" => "\u0301", "N" => "Ⅷ", "S" => "©", "Z" => "\u2028",
    "C" => "\u0378",
  }.each do |name, ch|
    assert_true Regexp.new("\\p{#{name}}").match?(ch), "\\p{#{name}} holds U+#{ch.ord.to_s(16)}"
    assert_false Regexp.new("\\p{#{name}}").match?("a"), "\\p{#{name}} does not hold a"
  end
  # The surrogates spell no character, so nothing a subject holds is one.
  assert_false(/\p{Cs}/.match?("a\u{d7ff}\u{e000}"))
end

assert("Regexp - \\p reads the emoji properties") do
  skip unless __ENCODING__ == "UTF-8"
  assert_true(/\A(\p{Emoji_Presentation}|\p{Extended_Pictographic})+\z/.match?("🔔🎉"))
  assert_false(/\A(\p{Emoji_Presentation}|\p{Extended_Pictographic})+\z/.match?("🔔x"))
  # Emoji holds the digits and '#', which have an emoji form; only a character
  # shown as emoji by default has Emoji_Presentation.
  assert_equal "1#", "a1#"[/\p{Emoji}+/]
  assert_true(/\p{Emoji}/.match?("©"))
  assert_false(/\p{Emoji_Presentation}/.match?("©"))
  assert_true(/\p{Emoji_Presentation}/.match?("\u{1f600}"))
  assert_true(/\p{extended pictographic}/.match?("🔔"))
  assert_true(/\p{EXTENDED_PICTOGRAPHIC}/.match?("🔔"))
  assert_true(/\p{Extended-Pictographic}/.match?("🔔"))
end

assert("Regexp - \\p in a class and against it") do
  skip unless __ENCODING__ == "UTF-8"
  s = "Aa1 _-€é́あ🔔"
  assert_equal 7, s.scan(/\P{L}/).length
  assert_equal 7, s.scan(/\p{^L}/).length
  assert_equal 7, s.scan(/[^\p{L}]/).length
  assert_equal 7, s.scan(/[\P{L}]/).length
  assert_equal ["A", "1", "_"], s.scan(/[\p{Lu}\p{Nd}_]/)
  assert_equal "Aa1éあ", s.scan(/[\p{Alpha}[:digit:]]/).join
  assert_equal 6, s.scan(/[^\p{L}\p{N}]/).length
  assert_equal "aé", s.scan(/[\p{L}&&\p{Ll}]/).join
  assert_equal "Aあ", s.scan(/[\p{L}&&\P{Ll}]/).join
  assert_equal "Wörld", "Wörld! 123"[/\A\p{Lu}\p{Ll}+/]
  assert_equal "WÖRLD! 123", "Wörld! 123".gsub(/\p{L}+/) { |w| w.upcase }
end

assert("Regexp - \\p under /i") do
  skip unless __ENCODING__ == "UTF-8"
  # A category folds as a class of its members does, so \p{Lu} under /i
  # holds the lower case letters as well, U+212A KELVIN SIGN among them.
  assert_true(/\p{Lu}/i.match?("e"))
  assert_true(/\p{Ll}/i.match?("É"))
  assert_true(/\p{Lu}/i.match?("\u{212a}"))
  assert_true(/\p{Lower}/i.match?("É"))
  # A property alone negates after the fold, as [^\p{Lu}] does, and so holds
  # no cased letter; one in a class is a set the fold closes, and holds them
  # all. CRuby draws the same line.
  %w[K k].each do |ch|
    assert_false(/\P{Lu}/i.match?(ch), "\\P{Lu}/i against #{ch}")
    assert_false(/\p{^Lu}/i.match?(ch), "\\p{^Lu}/i against #{ch}")
    assert_false(/[^\p{Lu}]/i.match?(ch), "[^\\p{Lu}]/i against #{ch}")
    assert_true(/[\P{Lu}]/i.match?(ch), "[\\P{Lu}]/i against #{ch}")
  end
  assert_true(/\P{Lu}/i.match?("1"))
end
