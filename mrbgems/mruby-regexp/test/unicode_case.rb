# Only compiled into mrbtest when the build defines MRB_REGEXP_UNICODE_CASE;
# see the gem's mrbgem.rake. Without the option every assertion here would
# fail, since /i then folds ASCII letters and nothing else.
assert("Regexp - Unicode case folding under /i") do
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
  # U+0103 folds from U+0102, which the range holds, so /i reaches it even
  # though it sits past the upper bound itself.
  assert_equal "ă", "ă".match(/[Ā-Ă]/i)[0]
  assert_nil "ą".match(/[Ā-Ă]/i)
  # A counterpart of a different byte length: U+212A folds to 'k' and U+017F
  # to 's', so the two sides of the choice are 3 and 1, and 2 and 1, bytes.
  assert_equal "K", "K".match(/k/i)[0]
  assert_equal "k", "k".match(/K/i)[0]
  assert_equal "ſ", "ſ".match(/s/i)[0]
  # Backreferences compare folded too.
  assert_equal "Āā", "Āā".match(/(Ā)\1/i)[0]
  # Without /i nothing folds.
  assert_nil "ā".match(/Ā/)
  assert_nil "K".match(/k/)
  # A fold that produces several codepoints is still not applied.
  assert_nil "ss".match(/ß/i)
end
