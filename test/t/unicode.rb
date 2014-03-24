# Test of the \u notation

assert('bare \u notation test') do
  # Mininum and maximum one byte characters
  assert_equal("\u0000", "\x00")
  assert_equal("\u007F", "\x7F")

  # Mininum and maximum two byte characters
  assert_equal("\u0080", "\xC2\x80")
  assert_equal("\u07FF", "\xDF\xBF")

  # Mininum and maximum three byte characters
  assert_equal("\u0800", "\xE0\xA0\x80")
  assert_equal("\uFFFF", "\xEF\xBF\xBF")

  # Four byte characters require the \U notation
end

assert('braced \u notation test') do
  # Mininum and maximum one byte characters
  assert_equal("\u{0000}", "\x00")
  assert_equal("\u{007F}", "\x7F")

  # Mininum and maximum two byte characters
  assert_equal("\u{0080}", "\xC2\x80")
  assert_equal("\u{07FF}", "\xDF\xBF")

  # Mininum and maximum three byte characters
  assert_equal("\u{0800}", "\xE0\xA0\x80")
  assert_equal("\u{FFFF}", "\xEF\xBF\xBF")

  # Mininum and maximum four byte characters
  assert_equal("\u{10000}",  "\xF0\x90\x80\x80")
  assert_equal("\u{10FFFF}", "\xF4\x8F\xBF\xBF")
end

# Test regular expressions only if implemented
begin
  Regexp
  have_regexp = true
rescue NameError
  have_regexp = false
end
if have_regexp then
  assert('Testing \u in regular expressions') do
    # The regular expression uses the unbraced notation where the string uses
    # the braced notation, and vice versa, so these tests will fail if the \u
    # modification is not applied

    # Test of unbraced \u notation in a regular expression
    assert_false(/\u0300/ =~ "\u{02FF}")
    assert_true( /\u0300/ =~ "\u{0300}")
    assert_false(/\u0300/ =~ "\u{0301}")

    # Test of braced \u notation in a regular expression
    assert_false(/\u{0300}/ =~ "\u02FF")
    assert_true( /\u{0300}/ =~ "\u0300")
    assert_false(/\u{0300}/ =~ "\u0301")
  end
end
