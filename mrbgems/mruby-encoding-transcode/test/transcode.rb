##
# String#encode, the encoding errors and Encoding.default_external

def transcode_bin(s)
  s.dup.force_encoding("BINARY")
end

assert("encoding errors") do
  assert_equal StandardError, EncodingError.superclass
  assert_equal EncodingError, Encoding::CompatibilityError.superclass
  assert_equal EncodingError, Encoding::ConverterNotFoundError.superclass
  assert_equal EncodingError, Encoding::InvalidByteSequenceError.superclass
  assert_equal EncodingError, Encoding::UndefinedConversionError.superclass
end

assert("Encoding.default_external and default_internal") do
  assert_equal "UTF-8", Encoding.default_external
  assert_nil Encoding.default_internal
  begin
    Encoding.default_external = "binary"
    assert_equal "ASCII-8BIT", Encoding.default_external
    Encoding.default_internal = "US-ASCII"
    assert_equal "US-ASCII", Encoding.default_internal
    # encode with no target converts to default_internal
    assert_equal "a?", "a\u3042".encode(undef: :replace)
  ensure
    Encoding.default_external = Encoding::UTF_8
    Encoding.default_internal = nil
  end
  assert_equal "UTF-8", Encoding.default_external
  assert_nil Encoding.default_internal
  assert_raise(ArgumentError) { Encoding.default_external = nil }
  assert_raise(ArgumentError) { Encoding.default_internal = "NOPE" }
end

assert("String#encode from UTF-8") do
  s = "a\u3042b"
  assert_equal "a\u3042b", s.encode("UTF-8")
  assert_equal "UTF-8", s.encode.encoding
  assert_equal "a?b", s.encode("US-ASCII", undef: :replace)
  assert_equal "a*b", s.encode("BINARY", undef: :replace, replace: "*")
  assert_equal "ASCII-8BIT", s.encode("BINARY", undef: :replace).encoding
  assert_equal "ASCII-8BIT", s.encode(Encoding::US_ASCII, undef: :replace).encoding
  assert_equal "abc", "abc".encode("US-ASCII")

  e = assert_raise(Encoding::UndefinedConversionError) { s.encode("US-ASCII") }
  assert_equal "U+3042 from UTF-8 to US-ASCII", e.message
  assert_equal "\u3042", e.error_char
  assert_equal "UTF-8", e.source_encoding_name
  assert_equal "US-ASCII", e.destination_encoding_name
  e = assert_raise(Encoding::UndefinedConversionError) { "\u{1F600}".encode("BINARY") }
  assert_equal "U+1F600 from UTF-8 to ASCII-8BIT", e.message
end

assert("String#encode with invalid bytes") do
  bad = "a\xffb"
  # same encoding: left as it is, unless invalid: :replace
  assert_equal "a\xffb", bad.encode("UTF-8")
  assert_equal "a\uFFFDb", bad.encode("UTF-8", invalid: :replace)
  assert_equal "a\uFFFDb", bad.encode(invalid: :replace)
  assert_equal "a?b", bad.encode("BINARY", invalid: :replace)
  # one replacement for each maximal subpart, as String#scrub counts them
  assert_equal "?a???????z", "\xE3\x81a\xff\xfe\xC0\xAF\xED\xA0\x80z".encode("BINARY", invalid: :replace)

  e = assert_raise(Encoding::InvalidByteSequenceError) { bad.encode("BINARY") }
  assert_equal "\"\\xFF\" on UTF-8", e.message
  assert_equal transcode_bin("\xff"), e.error_bytes
  assert_equal "UTF-8", e.source_encoding_name
  assert_equal "ASCII-8BIT", e.destination_encoding_name
  assert_false e.incomplete_input?
  e = assert_raise(Encoding::InvalidByteSequenceError) { "\xE3\x81a".encode("BINARY") }
  assert_equal "\"\\xE3\\x81\" followed by \"a\" on UTF-8", e.message
  e = assert_raise(Encoding::InvalidByteSequenceError) { "a\xE3\x81".encode("BINARY") }
  assert_equal "incomplete \"\\xE3\\x81\" on UTF-8", e.message
  assert_true e.incomplete_input?

  # an undefined character and an invalid byte are replaced by their own options
  assert_equal "??", "\u3042\xff".encode("US-ASCII", invalid: :replace, undef: :replace)
  assert_raise(Encoding::UndefinedConversionError) { "\u3042\xff".encode("US-ASCII", invalid: :replace) }
end

assert("String#encode from ASCII-8BIT and US-ASCII") do
  bin = transcode_bin("a\xffb")
  assert_equal bin, bin.encode("BINARY")
  assert_equal "a\uFFFDb", bin.encode("UTF-8", undef: :replace)
  assert_equal "UTF-8", bin.encode("UTF-8", undef: :replace).encoding
  e = assert_raise(Encoding::UndefinedConversionError) { bin.encode("UTF-8") }
  assert_equal "\"\\xFF\" from ASCII-8BIT to UTF-8", e.message
  assert_equal transcode_bin("\xff"), e.error_char
  assert_raise(Encoding::UndefinedConversionError) { "a\xffb".encode("UTF-8", "BINARY") }
  assert_equal "abc", transcode_bin("abc").encode("UTF-8")
  # an explicit src that is dst converts nothing, but tags the result as dst
  assert_equal "UTF-8", transcode_bin("é").encode("UTF-8", "UTF-8").encoding
  assert_equal "ASCII-8BIT", "é".encode("BINARY", "BINARY").encoding

  # a byte beyond ASCII is invalid in US-ASCII
  e = assert_raise(Encoding::InvalidByteSequenceError) { bin.encode("UTF-8", "US-ASCII") }
  assert_equal "\"\\xFF\" on US-ASCII", e.message
  assert_equal "a\uFFFDb", bin.encode("UTF-8", "US-ASCII", invalid: :replace)
end

assert("String#encode options") do
  assert_equal "aAb", "a\u3042b".encode("US-ASCII", fallback: { "\u3042" => "A" })
  assert_equal "a[3]b", "a\u3042b".encode("US-ASCII", fallback: ->(c) { "[#{c.bytesize}]" })
  def transcode_fallback(c) = "M"
  assert_equal "M", "\u3042".encode("US-ASCII", fallback: method(:transcode_fallback))
  o = Object.new
  def o.[](c) = "O"
  assert_equal "O", "\u3042".encode("US-ASCII", fallback: o)
  # a fallback that gives nil leaves the character to undef:
  assert_equal "?", "\u3042".encode("US-ASCII", fallback: {}, undef: :replace)
  assert_raise(Encoding::UndefinedConversionError) { "\u3042".encode("US-ASCII", fallback: {}) }
  assert_raise(ArgumentError) { "\u3042".encode("US-ASCII", fallback: { "\u3042" => "\u3044" }) }
  # a fallback that changes the receiver leaves the conversion to what it was
  s = ("\u3042" + "a" * 64) * 4
  r = s.encode("US-ASCII", fallback: ->(c) { s.replace("Z" * 1000); "?" })
  assert_equal(("?" + "a" * 64) * 4, r)
  # undef: :replace comes before fallback:, which is then not called
  called = false
  assert_equal "?", "\u3042".encode("US-ASCII", undef: :replace, fallback: ->(c) { called = true; "A" })
  assert_false called
  assert_equal "*", "\u3042".encode("US-ASCII", replace: "*", fallback: { "\u3042" => "A" })

  # replace: alone replaces an undefined character, but not an invalid byte
  assert_equal "a*", "a\u3042".encode("US-ASCII", replace: "*")
  assert_raise(Encoding::InvalidByteSequenceError) { "\xff".encode("US-ASCII", replace: "*") }
  assert_raise(Encoding::UndefinedConversionError) { "\u3042".encode("US-ASCII", invalid: :replace, replace: "*") }
  # the replacement is read before fallback: can change it
  r = "?".dup
  assert_equal "A?", "\u3042\xff".encode("US-ASCII", invalid: :replace, replace: r, fallback: ->(c) { r.replace("\u00e9"); "A" })
  # and has to be well formed, whether or not anything is replaced
  e = assert_raise(ArgumentError) { "a".encode("UTF-8", replace: "\xff") }
  assert_equal "replacement string is broken: \"\\xFF\" as UTF-8", e.message
  assert_equal transcode_bin("\xff"), transcode_bin("\xff").encode("BINARY", invalid: :replace, replace: transcode_bin("\xff"))

  assert_equal "&lt;a&amp;\"b&gt;", "<a&\"b>".encode("UTF-8", xml: :text)
  assert_equal "\"&lt;a&amp;&quot;b&gt;\"", "<a&\"b>".encode("UTF-8", xml: :attr)
  assert_equal "&lt;&#x3042;&gt;", "<\u3042>".encode("US-ASCII", xml: :text)
  assert_equal "&#xE9;", "\u00e9".encode("US-ASCII", xml: :text, fallback: { "\u00e9" => "e" })

  assert_equal "a\r\nb", "a\nb".encode("UTF-8", crlf_newline: true)
  assert_equal "a\rb", "a\nb".encode("UTF-8", newline: :cr)
  assert_equal "a\nb\nc", "a\r\nb\rc".encode("UTF-8", universal_newline: true)
  assert_equal "a\nb\nc", "a\r\nb\rc".encode("UTF-8", newline: :lf)
  assert_equal transcode_bin("a\r\nb"), transcode_bin("a\nb").encode("BINARY", crlf_newline: true)
  assert_raise(Encoding::ConverterNotFoundError) { "a".encode("UTF-8", universal_newline: true, crlf_newline: true) }

  assert_raise(ArgumentError) { "a".encode("UTF-8", newline: :foo) }
  assert_raise(ArgumentError) { "a".encode("UTF-8", xml: :foo) }
  assert_raise(ArgumentError) { "a".encode("UTF-8", invalid: :foo) }
  assert_raise(ArgumentError) { "a".encode("UTF-8", undef: true) }
  assert_raise(TypeError) { "\u3042".encode("US-ASCII", undef: :replace, replace: 1) }
  # the replacement has to be something the target holds
  assert_raise(Encoding::ConverterNotFoundError) { "\u3042".encode("US-ASCII", undef: :replace, replace: "\u00e9") }
  # what it does not know is ignored
  assert_equal "a", "a".encode("UTF-8", foo: 1)
end

assert("String#encode fallback: cannot change the copies the conversion reads") do
  # the copy of the receiver the loop walks, found and replaced together with
  # the receiver, so the buffer they shared is freed and reused
  s = "\u00e9" * 64
  src = ("\u00e9" * 64).freeze
  done = false
  fb = ->(c) {
    unless done
      done = true
      ObjectSpace.each_object(String) do |o|
        o.replace("A" * 300) if !o.frozen? && !o.equal?(s) && o == src
      end
      s.replace("B" * 300)
      GC.start
      (1..50).map { "C" * 127 }
    end
    "?"
  }
  assert_equal "?" * 64, s.encode("US-ASCII", fallback: fb)

  # the copy of replace:, which has to stay something the target holds
  key = "~R~".freeze
  rep = key.dup
  fb = ->(c) {
    ObjectSpace.each_object(String) do |o|
      o.replace("\u00fc") if !o.frozen? && !o.equal?(rep) && o == key
    end
    "#"
  }
  assert_equal "#~R~", "\u00e9\xff".encode("US-ASCII", invalid: :replace, replace: rep, fallback: fb)
end

assert("String#encode to an encoding it does not convert") do
  e = assert_raise(Encoding::ConverterNotFoundError) { "a".encode("EUC-JP") }
  assert_equal "code converter not found (UTF-8 to EUC-JP)", e.message
  e = assert_raise(Encoding::ConverterNotFoundError) { "a".encode("UTF-8", "EUC-JP") }
  assert_equal "code converter not found (EUC-JP to UTF-8)", e.message
  assert_raise(TypeError) { "a".encode(nil) }
end

assert("String#encode!") do
  s = "a\u3042".dup
  assert_same s, s.encode!("BINARY", undef: :replace)
  assert_equal transcode_bin("a?"), s
  assert_equal "ASCII-8BIT", s.encoding
  s = "a\u3042".dup
  assert_same s, s.encode!("UTF-8")
  assert_equal "a\u3042", s
  assert_raise(FrozenError) { "a".freeze.encode!("BINARY") }
  # with nothing to convert, the result is still tagged as the target
  s = transcode_bin("é")
  assert_same s, s.encode!("UTF-8", "UTF-8")
  assert_equal "UTF-8", s.encoding

  # encode leaves the receiver as it was
  s = "a\u3042"
  s.encode("BINARY", undef: :replace)
  assert_equal "a\u3042", s
end
