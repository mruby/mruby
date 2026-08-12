##
# String(Ext) Test

UTF8STRING = __ENCODING__ == "UTF-8"

assert('String#valid_encoding?') do
  assert_true "hello".valid_encoding?
  if UTF8STRING
    assert_true "あ".valid_encoding?
    assert_false "\xfe".valid_encoding?
    assert_false "あ\xfe".valid_encoding?
    assert_true "あ\xfe".b.valid_encoding?

    # Measuring a string of stray bytes marks it as one byte per character,
    # which is true of it and says nothing about whether it is valid.
    s = "a\x80"
    assert_equal 2, s.size
    assert_false s.valid_encoding?

    # RFC 3629 restrictions
    assert_false "\xC0\x80".valid_encoding?          # overlong NUL
    assert_false "\xC1\xBF".valid_encoding?          # overlong (< U+0080)
    assert_false "\xE0\x9F\xBF".valid_encoding?      # overlong (< U+0800)
    assert_false "\xED\xA0\x80".valid_encoding?      # surrogate U+D800
    assert_false "\xED\xBF\xBF".valid_encoding?      # surrogate U+DFFF
    assert_false "\xF0\x8F\xBF\xBF".valid_encoding?  # overlong (< U+10000)
    assert_false "\xF4\x90\x80\x80".valid_encoding?  # above U+10FFFF
    assert_false "\xF5\x80\x80\x80".valid_encoding?  # above U+10FFFF
    assert_true "\u{D7FF}".valid_encoding?           # last code point before surrogates
    assert_true "\u{E000}".valid_encoding?           # first code point after surrogates
    assert_true "\u{10FFFF}".valid_encoding?         # largest valid code point

    # The same sequences, measured before they are asked about: counting each
    # byte on its own is what marks the string one byte per character.
    ["\xC0\x80", "\xED\xA0\x80", "\xF5\x80\x80\x80"].each do |t|
      t.size
      assert_false t.valid_encoding?
    end
  else
    assert_true "\xfe".valid_encoding?
  end
end

assert('String#length of a binary string counts bytes') do
  # A byte-indexed string has one position per byte, which is what indexing it
  # already answered. Measuring it read the same string as UTF-8, so the length
  # disagreed with every index taken off it.
  if UTF8STRING
    s = "\u{1F600}".b   # F0 9F 98 80: four bytes, one character
    assert_equal 4, s.size
    assert_equal 4, s.length
    assert_equal 4, s.bytesize
    assert_equal 4, s.chars.size
    assert_equal "\x9F".b, s[1]
    assert_equal "\x80".b, s[3]
    assert_equal "\x80".b, s[-1]
    assert_equal "\xF0\x9F".b, s[0, 2]
    assert_equal "\x9F\x98".b, s.slice(1, 2)
    # the length is not cached as a property of the bytes: force_encoding can
    # hand the same bytes back to the UTF-8 reading
    s.force_encoding(Encoding::UTF_8)
    assert_equal 1, s.size
    t = "\u{1F600}"
    assert_equal 1, t.size
    t.force_encoding(Encoding::BINARY)
    assert_equal 4, t.size
  end
end

assert('String#reverse! on a binary string reverses bytes') do
  if UTF8STRING
    s = "\u{1F600}".b
    s.reverse!
    assert_equal "\x80\x98\x9F\xF0".b, s
    u = "a\u{1F600}b"
    u.reverse!
    assert_equal "b\u{1F600}a", u
  end
end

assert('String#encoding') do
  if UTF8STRING
    a = "あ"
    assert_equal Encoding::UTF_8, a.encoding
    assert_equal Encoding::BINARY, a.b.encoding
    assert_equal a, a.force_encoding(Encoding::BINARY)
    assert_equal a, a.force_encoding(Encoding::BINARY)
    assert_equal Encoding::BINARY, a.encoding
  else
    a = "hello"
    assert_equal Encoding::BINARY, a.encoding
  end
end

assert('String#encoding survives a copy') do
  # A copy holds the same bytes, so it is byte-indexed exactly when the string
  # it copies is. The copy used to come back UTF-8, which made `size` and every
  # offset computed from it read the bytes as characters again.
  if UTF8STRING
    a = "\u{1F600}".b   # F0 9F 98 80: four bytes, one character
    assert_equal Encoding::BINARY, a.dup.encoding
    assert_equal Encoding::BINARY, a.clone.encoding
    assert_equal Encoding::BINARY, a.freeze.dup.encoding
    b = "hello"
    b.replace(a)
    assert_equal Encoding::BINARY, b.encoding
    # and a copy of a UTF-8 string is still UTF-8: the flag is copied, not set
    c = "\u{1F600}"
    assert_equal Encoding::UTF_8, c.dup.encoding
    d = "x".b
    d.replace(c)
    assert_equal Encoding::UTF_8, d.encoding
  end
end
