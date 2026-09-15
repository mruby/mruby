PACK_IS_LITTLE_ENDIAN = "\x01\00".unpack('S')[0] == 0x01

def assert_pack tmpl, packed, unpacked
  t = tmpl.inspect
  assert "assert_pack" do
    assert_equal packed, unpacked.pack(tmpl), "#{unpacked.inspect}.pack(#{t})"
    assert_equal unpacked, packed.unpack(tmpl), "#{packed.inspect}.unpack(#{t})"
  end
end

# pack & unpack 'm' (base64)
assert('pack("m")') do
  assert_pack "m", "", [""]
  assert_pack "m", "AA==\n", ["\0"]
  assert_pack "m", "AAA=\n", ["\0\0"]
  assert_pack "m", "AAAA\n", ["\0\0\0"]
  assert_pack "m", "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXpBQkNERUZHSElKS0xNTk9QUVJT\nVFVWV1hZWg==\n", ["abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"]

  ary = ["abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"]
  assert_equal ary, "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXpBQkNERUZHSElKS0xNTk9QUVJT\nVFVWV1hZWg==\n".unpack("m")
  assert_equal ary, "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXpBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg==\n".unpack("m")

  assert_equal "QQ==\n", ["A", "B"].pack("m50")
  assert_equal ["A"], "QQ==\n".unpack("m50")
  assert_equal "QQ==Qg==", ["A", "B"].pack("m0 m0")
  assert_equal ["A", "B"], "QQ==Qg==".unpack("m10 m10")
  assert_pack "m0", "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXpBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg==", ["abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"]
end

# pack & unpack 'M' (Quoted-printable)
assert('pack("M")') do
  assert_pack "M", "123=\n", ["123"]
  assert_pack "M", "=3D\n", ["=\n"]
  assert_pack "M", "=E3=81=82=\n", ["あ"]

  assert_equal ["123"], "123=\n".unpack("M")
  assert_equal ["=\n"], "=3D\n".unpack("M")
  assert_equal ["あ"], "=E3=81=82=\n".unpack("M")
end

# pack & unpack 'u' (UU-encode)
assert('pack("u")') do
  # Basic string test with known good values
  assert_pack "u", "-2&5L;&\\L(%=O<FQD(0``\n`\n", ["Hello, World!"]

  # Empty string test
  assert_pack "u", "", [""]

  # Binary data test with known good values
  assert_pack "u", "%``$\"`_\\`\n`\n", ["\x00\x01\x02\x03\xFF"]

  # Single character test with corrected value
  assert_pack "u", "!00``\n`\n", ["A"]

  # Test with different line lengths
  short_data = "ABCD"
  assert_equal [short_data], [short_data].pack("u").unpack("u")

  # Test longer data (will span multiple lines)
  long_data = "A" * 50
  packed_long = [long_data].pack("u")
  assert_equal [long_data], packed_long.unpack("u")

  # Huge explicit line length must not overflow the buffer size calculation
  assert_equal ["ab"].pack("u"), ["ab"].pack("u2147483647")
  assert_equal ["ab"], ["ab"].pack("x124u2147483647")[124..-1].unpack("u")

  # The leading length character holds six bits, so a line carries at most 63
  # bytes; encoding works on whole 3-byte groups, so the length rounds down to
  # a multiple of 3. Under 3 it falls back to the default 45.
  data = "abcdefghij" * 12
  line_length = ->(n) { [data].pack("u#{n}").getbyte(0) - 32 }
  assert_equal 45, line_length.call(0)
  assert_equal 45, line_length.call(2)
  assert_equal 3, line_length.call(3)
  assert_equal 42, line_length.call(44)
  assert_equal 45, line_length.call(45)
  assert_equal 45, line_length.call(47)
  assert_equal 63, line_length.call(63)
  assert_equal 63, line_length.call(64)
  assert_equal 63, line_length.call(1000)

  # every line length must survive a round trip
  (0..70).each do |n|
    assert_equal [data], [data].pack("u#{n}").unpack("u"), "round trip failed for u#{n}"
  end

  # Test that packed data ends with zero-length line for non-empty input
  packed = ["test"].pack("u")
  # Check if last two characters are backtick and newline
  assert_equal "`\n", packed[-2, 2], "UU-encoded data should end with zero-length line"
end

# pack & unpack 'B'/'b'
assert('pack("B/b")') do
  assert_pack "b*", "\xFF\x00", ["1111111100000000"]
  assert_pack "b*", "\x01\x02", ["1000000001000000"]
  assert_pack "b3", "\x01", ["100"]

  assert_pack "B*", "\xFF\x00", ["1111111100000000"]
  assert_pack "B*", "\x01\x02", ["0000000100000010"]
end

# pack & unpack 'H'
assert('pack("H")') do
  assert_pack "H*", "01", ["3031"]
  assert_pack "H*", "\020", ["10"]
end

assert('pack("C")') do
  assert_pack "C*", "\x00\x01\x7F\x80\xFF", [0, 1, 127, 128, 255]
end

assert('pack("a")') do
  assert_equal "a", ["abc"].pack("a")
  assert_equal "abc", ["abc"].pack("a*")
  assert_equal "abc\0", ["abc"].pack("a4")

  assert_equal ["abc\0"], "abc\0".unpack("a4")
  assert_equal ["abc "], "abc ".unpack("a4")
end

assert('pack("A")') do
  assert_equal "a", ["abc"].pack("A")
  assert_equal "abc", ["abc"].pack("A*")
  assert_equal "abc ", ["abc"].pack("A4")

  assert_equal ["abc"], "abc\0".unpack("A4")
  assert_equal ["abc"], "abc ".unpack("A4")
end

# regression tests
assert('issue #1') do
  assert_equal "\000\001\000\002", [1, 2].pack("nn")
end

assert 'pack float' do
  skip unless Object.const_defined?(:Float)
  assert_pack 'e', "\x00\x00@@", [3.0]
  assert_pack 'g', "@@\x00\x00", [3.0]

  if PACK_IS_LITTLE_ENDIAN
    assert_pack 'f', "\x00\x00@@", [3.0]
    assert_pack 'F', "\x00\x00@@", [3.0]
  else
    assert_pack 'f', "@@\x00\x00", [3.0]
    assert_pack 'F', "@@\x00\x00", [3.0]
  end
end

assert 'pack double' do
  skip unless Object.const_defined?(:Float)
  assert_pack 'E', "\x00\x00\x00\x00\x00\x00\b@", [3.0]
  assert_pack 'G', "@\b\x00\x00\x00\x00\x00\x00", [3.0]

  if PACK_IS_LITTLE_ENDIAN
    assert_pack 'd', "\x00\x00\x00\x00\x00\x00\b@", [3.0]
    assert_pack 'D', "\x00\x00\x00\x00\x00\x00\b@", [3.0]
  else
    assert_pack 'd', "@\b\x00\x00\x00\x00\x00\x00", [3.0]
    assert_pack 'D', "@\b\x00\x00\x00\x00\x00\x00", [3.0]
  end
end

assert 'unpack a NaN that signals' do
  skip unless Object.const_defined?(:Float)
  # A NaN that signals has every bit it is set in the low payload, which is
  # where a build that keeps no object for a NaN writes what tells one from
  # another. Reading one back has to leave a NaN rather than the infinity an
  # empty payload under that exponent would be, and two reads have to leave
  # two objects, as they do for a NaN made any other way.
  s = "\x01\x00\x00\x00\x00\x00\xf0\x7f"
  a = s.unpack1("E")
  b = s.unpack1("E")

  assert_predicate(a, :nan?)
  assert_predicate(b, :nan?)
  assert_false(a.equal?(b))
end

assert 'pack/unpack "i"' do
  int_size = [0].pack('i').size
  raise "pack('i').size is too small (#{int_size})" if int_size < 2

  if PACK_IS_LITTLE_ENDIAN
    str = "\xC7\xCF" + "\xFF" * (int_size-2)
  else
    str = "\xFF" * (int_size-2) + "\xCF\xC7"
  end
  assert_pack 'i', str, [-12345]
end

assert 'pack/unpack "I"' do
  uint_size = [0].pack('I').size
  raise "pack('I').size is too small (#{uint_size})" if uint_size < 2

  if PACK_IS_LITTLE_ENDIAN
    str = "\x39\x30" + "\0" * (uint_size-2)
  else
    str = "\0" * (uint_size-2) + "\x30\x39"
  end
  assert_pack 'I', str, [12345]
end

assert 'pack/unpack "w"' do
  for x in [0,1,127,128,16383,16384,65535,65536]
    assert_equal [x], [x].pack("w").unpack("w")
  end
end

assert 'pack/unpack "U"' do
  assert_equal [], "".unpack("U")
  assert_equal [], "".unpack("U*")
  assert_equal [65, 66], "ABC".unpack("U2")
  assert_equal [12371, 12435, 12395, 12385, 12399, 19990, 30028], "こんにちは世界".unpack("U*")

  assert_equal "", [].pack("U")
  assert_equal "", [].pack("U*")
  assert_equal "AB", [65, 66, 67].pack("U2")
  assert_equal "こんにちは世界", [12371, 12435, 12395, 12385, 12399, 19990, 30028].pack("U*")

  assert_equal "\000", [0].pack("U")

  assert_raise(RangeError) { [-0x40000000].pack("U") }
  assert_raise(RangeError) { [-1].pack("U") }
end

assert 'pack("U") with a value past U+10FFFF' do
  # CRuby writes every value up to 0x7FFFFFFF, over up to six bytes, and
  # unpack("U") reads each of them back.
  [
    [0x10FFFF,   [0xF4, 0x8F, 0xBF, 0xBF]],
    [0x110000,   [0xF4, 0x90, 0x80, 0x80]],
    [0x1FFFFF,   [0xF7, 0xBF, 0xBF, 0xBF]],
    [0x200000,   [0xF8, 0x88, 0x80, 0x80, 0x80]],
    [0x3FFFFFF,  [0xFB, 0xBF, 0xBF, 0xBF, 0xBF]],
    [0x4000000,  [0xFC, 0x84, 0x80, 0x80, 0x80, 0x80]],
    [0x40000000, [0xFD, 0x80, 0x80, 0x80, 0x80, 0x80]],
    [0x7FFFFFFF, [0xFD, 0xBF, 0xBF, 0xBF, 0xBF, 0xBF]],
  ].each do |v, bytes|
    assert_equal bytes, [v].pack("U").unpack("C*"), v.to_s(16)
    assert_equal [v], [v].pack("U").unpack("U"), v.to_s(16)
  end
  # 0x7FFFFFFF + 1 is added at run time, since the literal does not fit
  # MRB_INT32 without bigint. The encoder's bound sees it only where it is an
  # mrb_int; a big integer is refused while the element is converted.
  top = 0x7FFFFFFF
  above = nil
  wide = begin
    above = top + 1  # RangeError where mrb_int is 32 bits and bigint is absent
    [][above]        # nil for an mrb_int index, RangeError for a big integer
    true
  rescue RangeError
    false
  end
  assert_raise(RangeError) { [above].pack("U") } if wide

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
  # A big integer is not an mrb_int either: `pack` refuses it while converting
  # the element, so the encoder never sees the value and the truncation this
  # guards against never runs.
  assert_raise(RangeError) { [wrapping].pack("U") } if wide
end

assert 'pack("U") with a UTF-16 surrogate' do
  # A surrogate has a spelling here even though it is not a character: CRuby
  # writes these three bytes too, and refuses the value in Integer#chr rather
  # than here. unpack("U") reads them back, so the two stay a pair whatever
  # the character scanner makes of the bytes.
  assert_equal [0xED, 0xA0, 0x80], [0xD800].pack("U").unpack("C*")
  assert_equal [0xED, 0xBF, 0xBF], [0xDFFF].pack("U").unpack("C*")
  assert_equal [0xD800], [0xD800].pack("U").unpack("U*")
  assert_equal [0xED, 0x9F, 0xBF], [0xD7FF].pack("U").unpack("C*")
  assert_equal [0xEE, 0x80, 0x80], [0xE000].pack("U").unpack("C*")
end

assert 'unpack("U") over every lead byte' do
  # Every lead byte followed by up to five continuation bytes, once with the
  # lowest continuation byte and once with the highest, so that every boundary
  # has a sequence on each side of it: a shorter spelling (C0, C1, E0 80,
  # F0 80, F8 80, FC 80), a surrogate (ED A0 and above), U+10FFFF (F4 90 and
  # above, F5 to F7), and the five and six byte lengths (F8 to FD). A shorter
  # spelling of a value is "redundant" and everything else short of a
  # character is "malformed", which is how CRuby tells the two apart, and
  # a value past U+10FFFF is read up to 0x7FFFFFFF, as CRuby reads it. The
  # next test holds the sequences right on either side of each bound on the
  # second byte.
  min = [0, 128, 2048, 65536, 2097152, 67108864]
  claim = ->(c) {
    if c < 0x80 then 1 elsif c < 0xC0 then 0 elsif c < 0xE0 then 2
    elsif c < 0xF0 then 3 elsif c < 0xF8 then 4 elsif c < 0xFC then 5
    elsif c < 0xFE then 6 else 0 end
  }
  # the fillers are continuation bytes, so only the count and the value can
  # fall short
  expected = ->(bytes) {
    c = bytes[0]
    n = claim.call(c)
    next [c] if n == 1
    next :malformed if n == 0 || bytes.size < n
    v = c & (0x7F >> n)
    (1...n).each {|k| v = (v << 6) | (bytes[k] & 0x3F) }
    next :redundant if v < min[n - 1]
    [v]
  }
  0.upto(255) do |c|
    [0x80, 0xBF].each do |f|
      0.upto(5) do |k|
        bytes = [c] + [f] * k
        got = begin
          bytes.pack("C*").unpack("U")
        rescue ArgumentError => e
          e.message.split(" ").first.to_sym
        end
        assert_equal expected.call(bytes), got, bytes.inspect
      end
    end
  end
end

assert 'unpack("U") on either side of each bound on the second byte' do
  read = ->(s) {
    begin
      s.unpack("U")
    rescue ArgumentError => e
      e.message.split(" ").first.to_sym
    end
  }
  # Each pair differs in the byte after the lead alone, one step across the
  # floor of three to six bytes, the surrogates and U+10FFFF.
  [
    ["\xE0\x9F\xBF", :redundant],             ["\xE0\xA0\x80", [0x800]],
    ["\xED\x9F\xBF", [0xD7FF]],               ["\xED\xA0\x80", [0xD800]],
    ["\xF0\x8F\xBF\xBF", :redundant],         ["\xF0\x90\x80\x80", [0x10000]],
    ["\xF4\x8F\xBF\xBF", [0x10FFFF]],         ["\xF4\x90\x80\x80", [0x110000]],
    ["\xF8\x87\xBF\xBF\xBF", :redundant],     ["\xF8\x88\x80\x80\x80", [0x200000]],
    ["\xFC\x83\xBF\xBF\xBF\xBF", :redundant], ["\xFC\x84\x80\x80\x80\x80", [0x4000000]],
  ].each do |s, v|
    assert_equal v, read.call(s), s.inspect
  end
  # A second byte below the floor says "redundant" only once every byte the
  # lead claims has continued it, as CRuby checks them.
  ["\xE0\x80\x41", "\xF0\x80\x80\x41", "\xF8\x80\x80\x80\x41", "\xFC\x80\x80\x80\x80\x41"].each do |s|
    assert_equal :malformed, read.call(s), s.inspect
  end
end

assert 'unpack("U") of a sequence cut short' do
  # CRuby says how many bytes the lead byte claims and how many are left.
  assert_raise_with_message(ArgumentError, "malformed UTF-8 character (expected 3 bytes, given 2 bytes)") {
    "a\xE3\x81".unpack("U*")
  }
  assert_raise_with_message(ArgumentError, "malformed UTF-8 character (expected 6 bytes, given 1 bytes)") {
    "\xFD".unpack("U")
  }
  assert_raise_with_message(ArgumentError, "malformed UTF-8 character") { "\xE3\x41\x81".unpack("U") }
end

assert 'unpack1' do
  d = 1234
  assert_equal(d, [d].pack("i").unpack1("i"))
  d = "foobar"
  assert_equal(d, [d].pack("a*").unpack1("a*"))
  assert_equal(d, [d].pack("A*").unpack1("A*"))
  assert_equal(d, [d].pack("Z*").unpack1("Z*"))
  assert_equal(d, [d].pack("m").unpack1("m"))
  assert_equal(d, [d].pack("M").unpack1("M"))
  d = "10010101"
  assert_equal(d, [d].pack("b*").unpack1("b*"))
  d = "f00b00"
  assert_equal(d, [d].pack("h*").unpack1("h*"))
end

assert 'unpack of a fixed size directive past the end' do
  # CRuby answers nil for each element a count asks for that the bytes left
  # cannot fill, whether a piece of one is left or nothing is, and none for
  # `*`. The position stays where the bytes ran short.
  assert_equal [97, nil], "a".unpack("CC")
  assert_equal [nil], "".unpack("C")
  assert_equal [nil, nil], "".unpack("C2")
  assert_equal [24930, nil], "abc".unpack("n2")
  assert_equal [97], "a".unpack("C*")
  assert_equal [24930], "abc".unpack("n*")
  assert_equal [97, nil, nil], "a".unpack("CnC")
  assert_equal [nil, 97], "a".unpack("nC")
  assert_equal [nil], "".unpack("e") if Object.const_defined?(:Float)
  # a directive without a fixed size answers nothing past the end
  assert_equal [], "".unpack("U")
  assert_equal [], "".unpack("w")
  assert_equal [""], "".unpack("a")
end
