# Tests for mruby-string-bitops
# Ported from CRuby [Feature #22118] test/ruby/test_string.rb

assert('String#bit_get') do
  s = "\xAA\x80"
  assert_equal 0, s.bit_get(0)
  assert_equal 1, s.bit_get(1)
  assert_equal 1, s.bit_get(7)
  assert_equal 1, s.bit_get(0, lsb_first: false)
  assert_equal 0, s.bit_get(1, lsb_first: false)
  assert_equal 1, s.bit_get(8, lsb_first: false)
  assert_nil s.bit_get(16)
  assert_raise(IndexError) { s.bit_get(-1) }
  assert_raise(ArgumentError) { s.bit_get(0, lsb_first: nil) }
  # CRuby raises ArgumentError for offsets too large to represent.
  # mruby has no Bignum bit offsets and raises RangeError instead:
  # without mruby-bigint the power itself overflows, with it the
  # offset conversion rejects the Bigint.  Either way this raises
  # in every build configuration (a float literal like 1e30 reads
  # as 0 under MRB_NO_FLOAT).
  assert_raise(RangeError) { s.bit_get(2 ** 100) }

  # The Float path of the offset conversion, built without a float
  # literal, which MRB_NO_FLOAT builds would read as 0.
  if 1.respond_to?(:to_f)
    huge = (1 << 30).to_f
    huge = huge * huge * 16  # 2.0**64, beyond mrb_int in any build
    assert_raise(RangeError) { s.bit_get(huge) }
  end

  # Unlike CRuby's rb_to_int, an offset is not converted with to_int:
  # mruby has no implicit conversion protocol in core, so an object
  # that only defines to_int is rejected here exactly as it is by
  # Array.new(obj), ary[obj] and "s" * obj.
  o = Object.new
  def o.to_int
    1
  end
  assert_raise(TypeError) { s.bit_get(o) }
  assert_raise(TypeError) { s.bit_get("1") }

end

assert('String#bit_set?') do
  s = "\xAA\x80"
  assert_equal false, s.bit_set?(0)
  assert_equal true, s.bit_set?(1)
  assert_equal true, s.bit_set?(7)
  assert_equal true, s.bit_set?(0, lsb_first: false)
  assert_equal false, s.bit_set?(1, lsb_first: false)
  assert_equal true, s.bit_set?(8, lsb_first: false)
  assert_nil s.bit_set?(16)
  assert_raise(IndexError) { s.bit_set?(-1) }
  assert_raise(ArgumentError) { s.bit_set?(0, lsb_first: nil) }
  assert_raise(RangeError) { s.bit_set?(2 ** 100) }
end

assert('String#bit_set, #bit_clear, #bit_flip') do
  s = "\x00"
  assert_equal s.object_id, s.bit_set(1).object_id
  assert_equal "\x02", s
  assert_equal s.object_id, s.bit_clear(1).object_id
  assert_equal "\x00", s
  assert_equal s.object_id, s.bit_flip(1).object_id
  assert_equal "\x02", s
  s.bit_flip(1)
  assert_equal "\x00", s

  s.bit_set(1, lsb_first: false)
  assert_equal "\x40", s
  s.bit_clear(1, lsb_first: false)
  assert_equal "\x00", s

  s = "\x00\x00"
  s.bit_set(8, lsb_first: false)
  assert_equal "\x00\x80", s
  s.bit_clear(8, lsb_first: false)
  assert_equal "\x00\x00", s
  s.bit_flip(8, lsb_first: false)
  assert_equal "\x00\x80", s

  assert_raise(IndexError) { "\x00".bit_set(8) }
  assert_raise(IndexError) { "\x00".bit_set(-1) }
  assert_raise(IndexError) { "\x00".bit_clear(8) }
  assert_raise(IndexError) { "\x00".bit_clear(-1) }
  assert_raise(IndexError) { "\x00".bit_flip(8) }
  assert_raise(IndexError) { "\x00".bit_flip(-1) }
  assert_raise(ArgumentError) { "\x00".bit_set(0, lsb_first: nil) }
  assert_raise(FrozenError) { "\x00".freeze.bit_set(0) }

  shared = "fooXbar".split("X").last
  shared.bit_set(0)
  assert_equal "car", shared
end

assert('String#bit_count') do
  assert_equal 0, "".bit_count
  assert_equal 0, "\x00".bit_count
  assert_equal 8, "\xFF".bit_count
  assert_equal 8, "\xAA\xF0".bit_count
  # lsb_first only changes the numbering within a byte; a whole-string
  # count is the same either way, and the keyword is accepted anyway.
  assert_equal 8, "\xAA\xF0".bit_count(lsb_first: false)
  assert_equal 8, "\xAA\xF0".bit_count(lsb_first: true)
  assert_raise(ArgumentError) { "\x00".bit_count(lsb_first: nil) }
  # No one-bit form: a lone offset is neither bit_get nor "to the end".
  assert_raise(ArgumentError) { "\x00".bit_count(0) }
end

assert('String#bit_set, #bit_clear, #bit_flip (region)') do
  # [Feature #22279]: (offset, length) and a Range name the same run.
  s = "\x00\x00\x00"
  assert_equal s.object_id, s.bit_set(4, 8).object_id
  assert_equal "\xF0\x0F\x00", s
  assert_equal "\xF0\x0F\x00", "\x00\x00\x00".bit_set(4..11)
  assert_equal "\xF0\x0F\x00", "\x00\x00\x00".bit_set(4...12)
  assert_equal s.object_id, s.bit_clear(6..9).object_id
  assert_equal "\x30\x0C\x00", s
  assert_equal s.object_id, s.bit_flip(0...24).object_id
  assert_equal "\xCF\xF3\xFF", s

  # A run within one byte, and one spanning whole middle bytes.
  assert_equal "\x3C", "\x00".bit_set(2, 4)
  assert_equal "\xC3", "\xFF".bit_clear(2, 4)
  assert_equal "\xC3", "\xFF".bit_flip(2, 4)
  assert_equal "\x80\xFF\xFF\x01", ("\x00" * 4).bit_set(7..24)
  assert_equal "\x7F\x00\x00\xFE", ("\xFF" * 4).bit_clear(7..24)
  assert_equal "\x7F\x00\x00\xFE", ("\xFF" * 4).bit_flip(7..24)
  # Long enough for the word-wide not kernel in the middle.
  assert_equal "\x01" + "\x00" * 40 + "\x80", ("\xFF" * 42).bit_flip(1...335)

  # Beginless and endless ranges reach the first and the last bit.
  assert_equal "\x0F\x00", "\x00\x00".bit_set(..3)
  assert_equal "\x00\xF0", "\x00\x00".bit_set(12..)
  assert_equal "\xFF\xFF", "\x00\x00".bit_set(0..)
  assert_equal "\xFF\xFF", "\x00\x00".bit_set(nil..nil)

  # lsb_first: false mirrors the run within each byte.
  assert_equal "\xF0", "\x00".bit_set(0, 4, lsb_first: false)
  assert_equal "\x1F\xF8", "\x00\x00".bit_set(3..12, lsb_first: false)
  assert_equal "\xE0\x07", "\xFF\xFF".bit_clear(3..12, lsb_first: false)
  assert_equal "\x1F\xF8", "\x00\x00".bit_flip(3..12, lsb_first: false)

  # An empty region is a no-op, but must start no later than the
  # position after the last bit, as "abc"[3, 0] is "" and "abc"[4, 0]
  # is nil.
  assert_equal "\x00", "\x00".bit_set(8, 0)
  assert_equal "\x00", "\x00".bit_set(8...8)
  assert_equal "\x00", "\x00".bit_set(3, 0)
  assert_equal "\x00", "\x00".bit_set(5..2)
  assert_equal "\x00", "\x00".bit_flip(8..)
  assert_raise(IndexError) { "\x00".bit_set(9, 0) }
  assert_raise(IndexError) { "\x00".bit_set(9...9) }
  assert_raise(IndexError) { "\x00".bit_set(9..) }

  # Overrun is refused before any bit changes.
  s = "\x00\x00"
  assert_raise(IndexError) { s.bit_set(12, 5) }
  assert_raise(IndexError) { s.bit_set(12..16) }
  assert_raise(IndexError) { s.bit_flip(0, 17) }
  assert_equal "\x00\x00", s
  assert_raise(IndexError) { "\x00".bit_set(-1, 1) }
  assert_raise(IndexError) { "\x00".bit_set(-1..2) }
  assert_raise(IndexError) { "\x00".bit_set(0..-1) }
  assert_raise(ArgumentError) { "\x00".bit_set(0, -1) }
  assert_raise(ArgumentError) { "\x00".bit_set(0..1, 2) }
  assert_raise(TypeError) { "\x00".bit_set(0, nil) }
  assert_raise(TypeError) { "\x00".bit_set(0, "1") }
  assert_raise(TypeError) { "\x00".bit_set("a".."b") }
  assert_raise(ArgumentError) { "\x00".bit_set(0, 1, lsb_first: nil) }

  # A frozen receiver is refused even for an empty region, but an
  # out-of-range one is still reported first.
  assert_raise(FrozenError) { "\x00".freeze.bit_set(0, 0) }
  assert_raise(FrozenError) { "\x00".freeze.bit_clear(0..3) }
  assert_raise(IndexError) { "\x00".freeze.bit_set(9, 0) }

  shared = "fooXbar".split("X").last
  shared.bit_set(0, 2)
  assert_equal "car", shared
end

assert('String#bit_count (region)') do
  s = "\xF0\x0F\xAA"
  assert_equal 12, s.bit_count(0, 24)
  assert_equal 12, s.bit_count(0..)
  assert_equal 0, s.bit_count(0, 4)
  assert_equal 4, s.bit_count(4, 4)
  assert_equal 8, s.bit_count(4..11)
  assert_equal 8, s.bit_count(4...12)
  assert_equal 4, s.bit_count(..7)
  assert_equal 4, s.bit_count(16..)
  assert_equal 2, s.bit_count(17..20)
  assert_equal 4, s.bit_count(0, 4, lsb_first: false)
  assert_equal 0, s.bit_count(4, 4, lsb_first: false)
  assert_equal 2, s.bit_count(17..20, lsb_first: false)
  # Long enough for the word-wide count in the middle.
  assert_equal 8 * 40 + 7 + 7, ("\xFF" * 42).bit_count(1...335)
  assert_equal 8 * 40 + 7 + 7, ("\xFF" * 42).bit_count(1...335, lsb_first: false)

  # A count clamps to the bits that exist and gives 0, not nil, for
  # an empty intersection: it feeds arithmetic such as length - count.
  assert_equal 4, "\xFF".bit_count(4, 100)
  assert_equal 4, "\xFF".bit_count(4..)
  assert_equal 0, "\xFF".bit_count(8, 4)
  assert_equal 0, "\xFF".bit_count(9, 0)
  assert_equal 0, "\xFF".bit_count(100..200)
  assert_equal 0, "\xFF".bit_count(3, 0)
  assert_equal 0, "\xFF".bit_count(5..2)
  assert_equal 0, "".bit_count(0..)

  assert_raise(IndexError) { "\xFF".bit_count(-1, 1) }
  assert_raise(IndexError) { "\xFF".bit_count(-1..2) }
  assert_raise(IndexError) { "\xFF".bit_count(0..-1) }
  assert_raise(ArgumentError) { "\xFF".bit_count(0, -1) }
  assert_raise(ArgumentError) { "\xFF".bit_count(0..1, 2) }
  assert_raise(TypeError) { "\xFF".bit_count(0, nil) }
  assert_raise(TypeError) { "\xFF".bit_count(nil, 1) }
  assert_raise(ArgumentError) { "\xFF".bit_count(0, 1, lsb_first: nil) }
end

assert('String#bit_count (long strings)') do
  # Exercise the word-at-a-time and unrolled paths.
  assert_equal 0, ("\x00" * 100).bit_count
  assert_equal 800, ("\xFF" * 100).bit_count
  assert_equal 400, ("\xAA" * 100).bit_count
  assert_equal 4 * 33, ("\x0F" * 33).bit_count
  # 24 bytes: embedded (and thus only 4-byte aligned on 64-bit
  # builds), exercising the unaligned memcpy word loop with no tail.
  assert_equal 8 * 24, ("\xFF" * 24).bit_count
  # 20 bytes: unaligned memcpy word loop plus a 4-byte tail on 64-bit.
  assert_equal 4 * 20, ("\x0F" * 20).bit_count
end

assert('String#bitwise_not and #bitwise_not!') do
  s = "\x00\xAA"
  result = s.bitwise_not
  assert_equal "\xFF\x55", result
  assert_not_equal s.object_id, result.object_id
  assert_equal "\x00\xAA", s

  assert_equal s.object_id, s.bitwise_not!.object_id
  assert_equal "\xFF\x55", s

  # Like CRuby, the non-bang result is a BINARY string; observable
  # only when mruby-encoding is in the build.
  if "".respond_to?(:encoding)
    assert_equal Encoding::BINARY, "\x00\xAA".bitwise_not.encoding
  end

  assert_raise(FrozenError) { "\x00".freeze.bitwise_not! }
end

assert('String#bitwise_and, #bitwise_or, #bitwise_xor') do
  assert_equal "\xC0", "\xF0".bitwise_and("\xCC")
  assert_equal "\xFC", "\xF0".bitwise_or("\x0C")
  assert_equal "\x3C", "\xF0".bitwise_xor("\xCC")

  s = "\xF0"
  assert_equal s.object_id, s.bitwise_and!("\xCC").object_id
  assert_equal "\xC0", s
  assert_equal s.object_id, s.bitwise_or!("\x0C").object_id
  assert_equal "\xCC", s
  assert_equal s.object_id, s.bitwise_xor!("\xFF").object_id
  assert_equal "\x33", s

  # Unlike CRuby's StringValue, an operand is not converted with
  # to_str; see the offset conversion for the same reasoning.
  o = Object.new
  def o.to_str
    "\xCC"
  end
  assert_raise(TypeError) { "\xF0".bitwise_and(o) }
  assert_raise(TypeError) { "\xF0".bitwise_and!(o) }

  if "".respond_to?(:encoding)
    assert_equal Encoding::BINARY, "\xF0".bitwise_and("\xCC").encoding
  end

  # Length mismatch: other longer, shorter, and empty.
  assert_raise(ArgumentError) { "\xF0".bitwise_and("\x00\x00") }
  assert_raise(ArgumentError) { "\xF0".bitwise_and("") }
  assert_raise(ArgumentError) { "\xF0".bitwise_or("\x00\x00") }
  assert_raise(ArgumentError) { "\xF0".bitwise_or("") }
  assert_raise(ArgumentError) { "\xF0".bitwise_xor("\x00\x00") }
  assert_raise(ArgumentError) { "\xF0".bitwise_xor("") }
  assert_raise(ArgumentError) { "\xF0".bitwise_and!("") }
  assert_raise(ArgumentError) { "\xF0".bitwise_or!("") }
  assert_raise(ArgumentError) { "\xF0".bitwise_xor!("") }
  assert_raise(ArgumentError) { "\x00\x00".bitwise_or!("\x00") }
  assert_raise(TypeError) { "\x00".bitwise_or(nil) }
  assert_raise(TypeError) { "\x00".bitwise_or(0) }
  q = Object.new
  def q.to_str
    "\x00"
  end
  assert_raise(TypeError) { "\x00".bitwise_or(q) }
  assert_raise(TypeError) { "\x00".bitwise_or!(q) }
  assert_raise(FrozenError) { "\x00".freeze.bitwise_xor!("\x00") }
end

assert('String bitwise operations (long strings)') do
  a = "\xAA" * 41
  b = "\x0F" * 41
  assert_equal "\x0A" * 41, a.bitwise_and(b)
  assert_equal "\xAF" * 41, a.bitwise_or(b)
  assert_equal "\xA5" * 41, a.bitwise_xor(b)
  assert_equal "\x55" * 41, a.bitwise_not
  assert_equal "\x00" * 41, a.bitwise_xor(a)

  c = a.dup
  c.bitwise_xor!(b)
  c.bitwise_xor!(b)
  assert_equal a, c

  # 20-byte strings are embedded (only 4-byte aligned on 64-bit
  # builds) and exercise the unaligned memcpy word loop of each
  # kernel, including a 4-byte tail.
  e = "\xAA" * 20
  f = "\x0F" * 20
  assert_equal "\x0A" * 20, e.bitwise_and(f)
  assert_equal "\xAF" * 20, e.bitwise_or(f)
  assert_equal "\xA5" * 20, e.bitwise_xor(f)
  assert_equal "\x55" * 20, e.bitwise_not
  e.bitwise_xor!(f)
  assert_equal "\xA5" * 20, e
end
