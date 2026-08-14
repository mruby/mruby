assert('sprintf invalid') do
  assert_raise(ArgumentError) { sprintf('%1$*d', 3) }
  assert_raise(ArgumentError) { sprintf('%1$.*d', 3) }
end

assert('String#%') do
  assert_equal "one=1", "one=%d" % 1
  assert_equal "1 one", "%d %s" % [ 1, "one" ]
  assert_equal "123 < 456", "%{num} < %<str>s" % { num: 123, str: "456" }
  assert_equal 15, ("%b" % (1<<14)).size
  skip unless Object.const_defined?(:Float)
  assert_equal "1.0", "%3.1f" % 1.01
  assert_equal " 12345.12", "% 4.2f" % 12345.1234
  assert_equal "12345.12", "%-4.2f" % 12345.12345
  assert_equal "+12345.12", "%+4.2f" % 12345.1234
  assert_equal "12345.12", "%04.2f" % 12345.12345
  assert_equal "0012345.12", "%010.2f" % 12345.1234
end

assert('String#% with inf') do
  skip unless Object.const_defined?(:Float)
  inf = Float::INFINITY

  assert_equal "Inf", "%f" % inf
  assert_equal "Inf", "%2f" % inf
  assert_equal "Inf", "%3f" % inf
  assert_equal " Inf", "%4f" % inf
  assert_equal "  Inf", "%5f" % inf

  assert_equal "+Inf", "%+f" % inf
  assert_equal "+Inf", "%+2f" % inf
  assert_equal "+Inf", "%+3f" % inf
  assert_equal "+Inf", "%+4f" % inf
  assert_equal " +Inf", "%+5f" % inf

  assert_equal "Inf", "%-f" % inf
  assert_equal "Inf", "%-2f" % inf
  assert_equal "Inf", "%-3f" % inf
  assert_equal "Inf ", "%-4f" % inf
  assert_equal "Inf  ", "%-5f" % inf

  assert_equal " Inf", "% f" % inf
  assert_equal " Inf", "% 2f" % inf
  assert_equal " Inf", "% 3f" % inf
  assert_equal " Inf", "% 4f" % inf
  assert_equal "  Inf", "% 5f" % inf
end

assert('String#% with nan') do
  skip unless Object.const_defined?(:Float)
  nan = Float::NAN

  assert_equal "NaN", "%f" % nan
  assert_equal "NaN", "%2f" % nan
  assert_equal "NaN", "%3f" % nan
  assert_equal " NaN", "%4f" % nan
  assert_equal "  NaN", "%5f" % nan

  assert_equal "+NaN", "%+f" % nan
  assert_equal "+NaN", "%+2f" % nan
  assert_equal "+NaN", "%+3f" % nan
  assert_equal "+NaN", "%+4f" % nan
  assert_equal " +NaN", "%+5f" % nan

  assert_equal "NaN", "%-f" % nan
  assert_equal "NaN", "%-2f" % nan
  assert_equal "NaN", "%-3f" % nan
  assert_equal "NaN ", "%-4f" % nan
  assert_equal "NaN  ", "%-5f" % nan

  assert_equal " NaN", "% f" % nan
  assert_equal " NaN", "% 2f" % nan
  assert_equal " NaN", "% 3f" % nan
  assert_equal " NaN", "% 4f" % nan
  assert_equal "  NaN", "% 5f" % nan
end

assert("String#% %b") do
  assert_equal("..10115", "%0b5" % -5)
end

assert("String#% %d") do
  assert_equal("  10",   "%4d" % 10)
  assert_equal("1000",   "%4d" % 1000)
  assert_equal("10000",  "%4d" % 10000)
end

assert("String#% invalid format") do
  assert_raise ArgumentError do
    "%?" % ""
  end
end

assert("sprintf %g with high precision") do
  # Regression test: precision values larger than double's significand
  # used to cause out-of-bounds reads in fp_uscale's fixed_width().
  skip unless Object.const_defined?(:Float)
  assert_equal "7",                                                "%.*g" % [51, 7]
  assert_equal "7.5",                                              "%.*g" % [51, 7.5]
  assert_equal "7",                                                "%.51g" % 7.0
  assert_equal "7." + "0" * 50,                                    "%#.51g" % 7.0
  assert_equal "7",                                                "%.*g" % [1000, 7.0]
end

assert("sprintf rejects an oversized float precision/width") do
  # A precision/width near INT_MAX used to allocate ~2GB and overflow the
  # formatter's int length into a negative memmove size, segfaulting on input
  # like "%.2147483647e" (clusterfuzz). It must raise instead.
  skip unless Object.const_defined?(:Float)
  assert_raise(ArgumentError) { sprintf("%.2147483647e", 131072) }
  assert_raise(ArgumentError) { sprintf("%2147483647e", 1.0) }
  assert_raise(ArgumentError) { sprintf("%.2147483647f", 1.0) }
  # ordinary precision/width still work
  assert_equal "1.31072e+05", sprintf("%.5e", 131072.0)
  assert_equal "3.1400000000", sprintf("%.10f", 3.14)
end

assert("sprintf with to_s mutating format string") do
  # The to_s callback must not be able to invalidate sprintf's internal
  # iteration pointers by mutating the format string.
  fmt = "%s" + "B" * 200
  mutator = Object.new
  $sprintf_test_fmt = fmt
  def mutator.to_s
    $sprintf_test_fmt.replace("Z")
    "ok"
  end
  result = sprintf(fmt, mutator)
  assert_equal 202, result.length
  assert_equal "ok", result[0, 2]
  assert_equal "B" * 200, result[2..]
end

assert('sprintf("%c") with an integer that has no UTF-8 encoding') do
  skip unless __ENCODING__ == "UTF-8"
  # Nothing was written to the encoder's buffer for these, and %c used to emit
  # whatever byte the stack happened to hold there.
  assert_raise(ArgumentError) { sprintf("%c", 0x110000) }
  assert_raise(ArgumentError) { sprintf("%c", -1) }
  # A value that would land inside the Unicode range if it were truncated to
  # 32 bits must not come out as the character it truncates to.
  # The shift width comes from a variable because `1 << 32` written out is
  # constant folded, and the fold fails while this file is compiled on
  # MRB_INT32 without bigint, dropping every test in it.
  shift = 32
  wrapping = nil
  wide = begin
    wrapping = (1 << shift) + 0x41  # RangeError where mrb_int is 32 bits and bigint is absent
    [][wrapping]                    # nil for an mrb_int index, RangeError for a big integer
    true
  rescue RangeError
    false
  end
  # A big integer is not an mrb_int either: `%c` takes it down the branch for
  # an argument that is not an integer and refuses it there, so the encoder
  # never sees the value and the truncation this guards against never runs.
  assert_raise(ArgumentError) { sprintf("%c", wrapping) } if wide
end

assert('sprintf("%c") with a UTF-16 surrogate') do
  skip unless __ENCODING__ == "UTF-8"
  # A surrogate has a spelling here even though it is not a character: CRuby
  # writes these three bytes too, and refuses the value in Integer#chr rather
  # than here. So what the encoder writes is wider than what the character
  # scanner reads back, and the string it builds is not valid UTF-8.
  assert_equal "\xED\xA0\x80", sprintf("%c", 0xD800)
  assert_equal "\xED\xBF\xBF", sprintf("%c", 0xDFFF)
  assert_equal "\xED\x9F\xBF", sprintf("%c", 0xD7FF)
  assert_equal "\xEE\x80\x80", sprintf("%c", 0xE000)
end

assert('what the string sprintf builds claims') do
  # The bytes go through: a byte-read argument lands in the result whole, and
  # a byte-read format string lays its own bytes down as they are. The reading
  # goes with them now: the format string's own reading is what the result is
  # built with, and an argument read as bytes and going above ASCII hands it
  # over the way any appended byte-read bytes do. Whether a string is read as
  # bytes or as UTF-8 is only visible through mruby-encoding, so ask only
  # where it is present.
  skip unless "".respond_to?(:encoding)
  skip unless __ENCODING__ == "UTF-8"
  bin = 171.chr   # a byte spelling no character, read as bytes
  assert_equal [171], ("%s" % [bin]).bytes
  ["%s" % [bin], "[%s]" % [bin], "%10s" % [bin], "%-10s" % [bin],
   "%s %s" % [bin, "x"], "%<x>s" % {x: bin}, "%{x}" % {x: bin},
   "%c" % [bin], sprintf("%s", bin)].each do |s|
    assert_equal Encoding::BINARY, s.encoding
    assert_true s.valid_encoding?
  end
  # what an argument is read as is a property of the argument, so precision
  # cutting the byte above ASCII off the written part moves nothing
  assert_equal Encoding::BINARY, ("%.1s" % ["a\xABb".force_encoding(Encoding::BINARY)]).encoding
  # a byte-read format string, with nothing written into it and with an
  # argument written into it
  assert_equal Encoding::BINARY, ("ab".force_encoding(Encoding::BINARY) % []).encoding
  assert_equal Encoding::BINARY, ("%s".force_encoding(Encoding::BINARY) % ["ab"]).encoding
  # what says nothing about the reading either way: ASCII bytes read the same
  # under any reading, an Integer is a code point, and inspect builds a string
  # of its own
  assert_equal Encoding::UTF_8, ("%s" % ["ab".force_encoding(Encoding::BINARY)]).encoding
  assert_equal Encoding::UTF_8, ("%c" % [171]).encoding
  assert_equal Encoding::UTF_8, ("%d" % [171]).encoding
  assert_equal Encoding::UTF_8, ("%p" % [bin]).encoding
  # A byte-read format string of nothing but ASCII keeps the byte reading even
  # where the argument is UTF-8 and goes above ASCII. CRuby answers UTF-8 here:
  # its rule lets an all-ASCII side yield to the other one, and this one never
  # takes the byte reading back off a string that carries it. `"".b << "あ"`
  # is the same cell, answered the same way on purpose.
  assert_equal Encoding::BINARY, ("%s".force_encoding(Encoding::BINARY) % ["あ"]).encoding
end
