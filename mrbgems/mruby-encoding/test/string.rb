##
# String(Ext) Test

UTF8STRING = __ENCODING__ == "UTF-8"
UNICODECASE = "\u00C4".downcase == "\u00E4"

assert('String#valid_encoding? survives what the string goes through') do
  # The answer is remembered on the string, either way it came out, so every
  # way of changing its bytes has to forget it again, and the two places a copy
  # keeps the bytes whole have to carry it along. Each pair asks the same
  # question of the same bytes, once with the answer already remembered and
  # once without. The bases below start out both valid and broken, and the
  # changes below both break and repair, so each remembered answer is asked
  # for after a change that unmakes it.
  if UTF8STRING
    [->(s) { s << "\x80" },
     ->(s) { s.concat("\xC0") },
     ->(s) { s.replace("\xED\xA0\x80") },
     ->(s) { s.insert(0, "\xFE") },
     ->(s) { s.prepend("\x81") },
     ->(s) { s.sub!("あ", "\xE0\x80"); s },
     ->(s) { s.gsub!("あ", "\xF5"); s },
     ->(s) { s.chop!; s },
     ->(s) { s.downcase!; s },
     ->(s) { s * 2 },
     ->(s) { s.dup },
     ->(s) { s.byteslice(0, 1) || s },
     ->(s) { s[0, 1] || s },
     ->(s) { s << "\x82" },
     ->(s) { s.replace("ok") },
     ->(s) { s * 0 },
     ->(s) { s.b },
     ->(s) { s.clear; s }].each do |op|
      ["あ", "あa", "あい", "あ" * 40,
       "\x80", "a\x80b", "\xE3\x81", "\xED\xA0\x80", "\xE3\x81" * 40].each do |base|
        warm = base.dup
        warm.valid_encoding?          # remember the answer before the change
        cold = base.dup
        a = (op.call(warm) rescue warm)
        b = (op.call(cold) rescue cold)
        assert_equal b.bytes, a.bytes
        assert_equal b.valid_encoding?, a.valid_encoding?
      end
    end
  end
end

assert('String#valid_encoding? after an append inside a shared buffer') do
  # An append to a string sharing a buffer with room to spare writes in place
  # rather than detaching, and that path forgets the remembered answer on its
  # own rather than through mrb_str_modify(). Nothing else here
  # reaches it: a string built by `*` or from a literal has no spare capacity,
  # so its sharers all take the detaching path instead.
  #
  # The append also has to be the one that makes a broken string valid, by
  # completing a sequence cut short at the end. An append that leaves the
  # string broken agrees with a stale answer and shows nothing.
  if UTF8STRING
    base = ""
    300.times { base << "a" }   # built up, so the allocation is wider than the string
    base << "\xE3\x81"          # a three-byte sequence two bytes in
    warm = base.dup             # shares the buffer, spare capacity and all
    assert_false warm.valid_encoding?
    warm << "\x82"              # completes it: the same bytes now spell a character
    assert_true warm.valid_encoding?
    cold = base.dup
    cold << "\x82"
    assert_equal cold.bytes, warm.bytes
    # and the other way round: an append that breaks a string it shares
    base2 = ""
    300.times { base2 << "a" }
    ok = base2.dup
    assert_true ok.valid_encoding?
    ok << "\xE3\x81"
    assert_false ok.valid_encoding?
  end
end

assert('String#valid_encoding?') do
  assert_true "hello".valid_encoding?
  if UTF8STRING
    assert_true "あ".valid_encoding?
    assert_false "\xfe".valid_encoding?
    assert_false "あ\xfe".valid_encoding?
    assert_true "あ\xfe".b.valid_encoding?

    # Measuring a string counts a byte that spells no character as a position
    # of its own, and marks nothing about the string that this answer is read
    # off.
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

    # The same sequences, measured before they are asked about
    ["\xC0\x80", "\xED\xA0\x80", "\xF5\x80\x80\x80"].each do |t|
      t.size
      assert_false t.valid_encoding?
    end
  else
    assert_true "\xfe".valid_encoding?
  end
end

assert('String#valid_encoding? answers the same however the string was read first') do
  # A string read once is marked as holding one character per byte where every
  # byte of it is ASCII, and the answer here is taken off that mark rather than
  # walked for. Nothing else a reading leaves behind may reach it, so ask the
  # same question of the same bytes twice, once after a reading and once not.
  if UTF8STRING
    readings = [->(s) { s.length }, ->(s) { s.chars }, ->(s) { s[0] },
                ->(s) { s.each_char { |c| } }, ->(s) { s.inspect },
                ->(s) { s.ord rescue nil }, ->(s) { s.codepoints rescue nil }]
    ["", "abc", "a" * 40, "あ", "あa", "a" * 40 + "\xfe", "\x80", "a\x80b",
     "\xE3\x81", "\xC0\xAF", "\xED\xA0\x80", "\xF4\x90\x80\x80",
     171.chr, 0xE3.chr, 65.chr].each do |base|
      want = base.dup.valid_encoding?
      readings.each do |read|
        s = base.dup
        read.call(s)
        assert_equal want, s.valid_encoding?, "#{base.inspect} read first"
      end
    end
  end
end

assert('a string that says it is UTF-8 and says it is valid spells its own bytes') do
  # The encoding a string reports and the answer it gives for its own validity
  # are kept apart on it, and either may be settled by a reading that never
  # asked the other. Where the two say the bytes are UTF-8 and read as it, the
  # code points taken off them have to spell those same bytes back.
  if UTF8STRING
    ["", "abc", "あ", "あa\u{1F600}", "\x80", "a\x80b", "\xED\xA0\x80",
     "\xE3\x81", 171.chr, 0xE3.chr, 65.chr, "あ".b, "abc".b].each do |base|
      [->(s) { s }, ->(s) { s.length; s }, ->(s) { s.inspect; s },
       ->(s) { s.chars; s }].each do |read|
        s = read.call(base.dup)
        next unless s.encoding == Encoding::UTF_8 && s.valid_encoding?
        back = s.codepoints.map { |cp| cp.chr("UTF-8") }.join
        assert_equal s.bytes, back.bytes, "#{base.inspect} read first"
      end
    end
  end
end

assert('String#valid_encoding? after a run of ASCII') do
  # The walk skips ASCII a word at a time and decodes only where a byte leaves
  # that range, so a broken byte has to be caught after such a run as well as
  # at the head of the string.
  if UTF8STRING
    assert_true ("a" * 40).valid_encoding?
    assert_true ("a" * 40 + "あ").valid_encoding?
    assert_false ("a" * 40 + "\xfe").valid_encoding?
    assert_false ("a" * 40 + "\xe3\x81").valid_encoding?  # 3-byte sequence cut short
    assert_true ("a" * 40 + "\xe3\x81").b.valid_encoding?
  end
end

assert('String#valid_encoding? of a shared substring') do
  # A substring too long to embed shares the parent's buffer, so the walk has
  # to stop where the substring ends rather than where the parent's bytes do.
  if UTF8STRING
    parent = "あ" * 40 + "\xfe"
    assert_true parent.byteslice(0, 120).valid_encoding?
    assert_false parent.byteslice(0, 121).valid_encoding?
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

assert('String#reverse! leaves what the bytes read as standing') do
  # Reversing puts the same bytes back with every character whole, so a string
  # that read as UTF-8 still does. A string that did not is the one case the
  # reversal can settle either way, since bytes that spell nothing where they
  # stood can spell a character once they are turned around. The reversal is
  # where all of that is decided; asking afterwards is how it is seen.
  if UTF8STRING
    a = "あいうz"
    a.reverse!
    assert_equal "zういあ", a
    assert_equal 4, a.length
    assert_true a.valid_encoding?
    a.reverse!
    assert_equal "あいうz", a
    assert_equal 4, a.length
    assert_true a.valid_encoding?

    b = "abc"
    b.reverse!
    assert_equal 3, b.length
    assert_true b.valid_encoding?

    c = "a\xE3\x81"
    c.reverse!
    assert_equal "\x81\xE3a".b, c.b
    assert_false c.valid_encoding?

    d = "\x80\xC2"   # a trailing byte and then a lead byte, spelling nothing
    assert_false d.valid_encoding?   # asked here, so the answer is on the string
    d.reverse!                       # and the same bytes now spell U+0080
    assert_equal "\xC2\x80".b, d.b
    assert_equal 1, d.length
    assert_true d.valid_encoding?
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

assert('what a string built out of a byte-read string claims') do
  # A copy carries the byte reading with the bytes, and so does everything
  # else here that builds a string out of them: the pieces cut out of the
  # string, its repetitions and sums, and the strings its bytes are shoveled
  # or spliced into, the pads included.
  if UTF8STRING
    b = "\xE3\x81\x82".b   # the bytes of a three-byte character, read as bytes
    assert_equal Encoding::BINARY, b.dup.encoding
    assert_equal Encoding::BINARY, b.reverse.encoding
    # a piece cut out of the string, and a repetition of it
    assert_equal Encoding::BINARY, b[0].encoding
    assert_true b[0].valid_encoding?
    assert_equal Encoding::BINARY, b.chars[0].encoding
    assert_equal Encoding::BINARY, (171.chr * 2).encoding
    assert_true (171.chr * 2).valid_encoding?
    # a sum with a byte-read operand on either side
    assert_equal Encoding::BINARY, (171.chr + 171.chr).encoding
    assert_true (171.chr + 171.chr).valid_encoding?
    assert_equal Encoding::BINARY, ("abc" + 171.chr).encoding
    # a string the bytes were shoveled or spliced into
    s = ""
    s << 171.chr
    assert_equal Encoding::BINARY, s.encoding
    assert_true s.valid_encoding?
    assert_equal Encoding::BINARY, [171.chr, 171.chr].join.encoding
    assert_equal Encoding::BINARY, "ab".gsub("a", 171.chr).encoding
    # the receiver's bytes in wider clothes are read the way the receiver was
    assert_equal Encoding::BINARY, 171.chr.ljust(3).encoding
    assert_equal Encoding::BINARY, 171.chr.rjust(3).encoding
  end
end

assert('a string cut out of a byte-read string') do
  # A subrange of a byte-read string holds nothing but bytes of it, so it is
  # read the same way. Every piece used to come back UTF-8, handing bytes that
  # spell no character a claim they could not honor.
  if UTF8STRING
    b = "\xE3\x81\x82".b   # the bytes of a three-byte character, read as bytes
    each_char_pieces = []
    b.each_char { |c| each_char_pieces << c }
    [b[0], b[0, 2], b[1..-1], b.chars[1], each_char_pieces[2],
     b.byteslice(0, 2), b.dup.slice!(0)].each do |piece|
      assert_equal Encoding::BINARY, piece.encoding
      assert_true piece.valid_encoding?
    end
    assert_equal [0xE3], b[0].bytes
    assert_equal [0x81, 0x82], b[1..-1].bytes
    # what Integer#chr hands back for a stray byte stays byte-read when cut
    assert_equal Encoding::BINARY, 171.chr[0].encoding
    # splitting on a byte-read separator cuts byte-read pieces, and so does
    # splitting on line ends
    assert_equal Encoding::BINARY, b.split(b[1])[0].encoding
    assert_equal Encoding::BINARY, "a\nb".b.lines[0].encoding
    # a piece of a string read as UTF-8 goes on reading as UTF-8
    assert_equal Encoding::UTF_8, "あい"[1].encoding
    assert_equal "い", "あい"[1]
  end
end

assert('a string built by repeating a byte-read string') do
  # `*` lays the same bytes down over again, so what it builds is read the
  # way the receiver was.
  if UTF8STRING
    s = 171.chr * 2
    assert_equal [171, 171], s.bytes
    assert_equal Encoding::BINARY, s.encoding
    assert_true s.valid_encoding?
    assert_equal Encoding::BINARY, ("abc".b * 2).encoding
    assert_equal Encoding::UTF_8, ("あ" * 2).encoding
  end
end

assert('String#+ with a byte-read operand') do
  if UTF8STRING
    bin = 171.chr   # a byte spelling no character, read as bytes
    # a byte-read operand carrying a byte above ASCII hands the sum bytes no
    # other reading holds, so its reading wins
    [bin + bin, "abc" + bin, bin + "abc", "" + bin].each do |s|
      assert_equal Encoding::BINARY, s.encoding
      assert_true s.valid_encoding?
    end
    assert_equal [97, 98, 99, 171], ("abc" + bin).bytes
    # CRuby refuses these pairs outright; here the sum says nothing rather
    # than something false
    assert_equal Encoding::BINARY, ("あ" + bin).encoding
    assert_equal Encoding::BINARY, (bin + "あ").encoding
    # a byte-read operand of ASCII bytes reads as the other operand as it
    # stands, so it yields to it
    assert_equal Encoding::UTF_8, ("abc".b + "あ").encoding
    assert_equal Encoding::UTF_8, ("あ" + "abc".b).encoding
    assert_true ("abc".b + "あ").valid_encoding?
    # two byte-read operands stay byte-read even over ASCII bytes
    assert_equal Encoding::BINARY, ("abc".b + "def".b).encoding
    # and two UTF-8 operands are what they were
    assert_equal Encoding::UTF_8, ("あ" + "い").encoding
  end
end

assert('String#+ over two all-ASCII operands is not CRuby') do
  # Where both operands hold nothing but ASCII, CRuby keeps the receiver's
  # encoding and answers ASCII-8BIT for the first case below. The rule here is
  # symmetric in the operands instead: the one bit tracked says "bytes read as
  # bytes landed here", and ASCII bytes never say that, whichever side they
  # came from.
  #
  # This is where + and << part company, and on purpose. << changes a string
  # that was already being read some way and never takes that reading back
  # off; + builds one that was not being read at all. Following CRuby on +
  # alone would put it at odds with join, which builds a fresh string the same
  # way, and following it on join too would mean dropping the byte reading off
  # a string that carries it.
  if UTF8STRING
    assert_equal Encoding::UTF_8, ("abc".b + "def").encoding
    assert_equal Encoding::UTF_8, ("abc" + "def".b).encoding
    assert_equal Encoding::UTF_8, ["abc".b, "def"].join.encoding
    # the receiver of << keeps what it had, for the same pair
    s = "abc".b
    s << "def"
    assert_equal Encoding::BINARY, s.encoding
    # none of this moves once a byte above ASCII is in play on the byte-read
    # side, which is the case the rule exists for
    assert_equal Encoding::BINARY, ("a\xABz".b + "def").encoding
    assert_equal Encoding::BINARY, ("abc" + "a\xABz".b).encoding
    assert_equal Encoding::BINARY, ["a\xABz".b, "def"].join.encoding
  end
end

assert('bytes shoveled or spliced into a string') do
  # Bytes that were read as bytes and go above ASCII spell no character in
  # the string they are appended or spliced into, so they hand it the byte
  # reading along with themselves. ASCII bytes read the same under any
  # reading and move nothing.
  if UTF8STRING
    s = ""
    s << 171.chr
    assert_equal Encoding::BINARY, s.encoding
    assert_true s.valid_encoding?
    assert_equal [171], s.bytes
    # concat and interpolation reach the same append
    t = "abc"
    t.concat(171.chr)
    assert_equal Encoding::BINARY, t.encoding
    assert_equal Encoding::BINARY, "<#{171.chr}>".encoding
    # so do join and the replacement gsub splices in
    assert_equal Encoding::BINARY, [171.chr, 171.chr].join.encoding
    assert_equal Encoding::BINARY, ["a", 171.chr].join("-").encoding
    assert_equal Encoding::BINARY, "a\x80b".b.gsub("a", "-").encoding
    assert_equal Encoding::BINARY, "ab".gsub("a", 171.chr).encoding
    # ASCII bytes say nothing about the reading
    u = ""
    u << "abc".b
    assert_equal Encoding::UTF_8, u.encoding
    # an Integer is read as a code point, and a code point is a character
    v = ""
    v << 171
    assert_equal Encoding::UTF_8, v.encoding
    assert_equal [0xC2, 0xAB], v.bytes
    # a byte-read receiver reads everything as bytes already; CRuby lifts one
    # of ASCII bytes to the argument's reading, which is a claim this string
    # never makes, so it stays as it is
    w = "".b
    w << "あ"
    assert_equal Encoding::BINARY, w.encoding
    assert_equal [0xE3, 0x81, 0x82], w.bytes
    # a splice into the middle or the front is the same landing
    x = "ab"
    x.insert(1, 171.chr)
    assert_equal Encoding::BINARY, x.encoding
    assert_equal [97, 171, 98], x.bytes
    y = "ab"
    y.prepend(171.chr)
    assert_equal Encoding::BINARY, y.encoding
    z = "ab"
    z[0, 1] = 171.chr
    assert_equal Encoding::BINARY, z.encoding
    assert_true z.valid_encoding?
    # and a splice of ASCII bytes moves nothing, wherever it lands
    za = "ab"
    za.insert(1, "x".b)
    za.prepend("y".b)
    za[0, 1] = "z".b
    assert_equal Encoding::UTF_8, za.encoding
  end
end

assert('String#append_as_bytes leaves the reading alone') do
  # append_as_bytes takes only the bytes of what it is given; the receiver's
  # reading does not move, whatever lands in it. CRuby specifies exactly this.
  if UTF8STRING
    s = "あ"
    s.append_as_bytes(171)
    assert_equal Encoding::UTF_8, s.encoding
    assert_false s.valid_encoding?
    assert_equal [0xE3, 0x81, 0x82, 171], s.bytes
    t = "あ"
    t.append_as_bytes(171.chr)
    assert_equal Encoding::UTF_8, t.encoding
    assert_equal [0xE3, 0x81, 0x82, 171], t.bytes
  end
end

assert('a byte-read string padded to width') do
  # ljust, rjust and center hand back the receiver's bytes in wider clothes,
  # so the result is read the way the receiver was, ASCII bytes and all. A
  # pad read as bytes marks the result the way any appended byte-read bytes
  # do.
  if UTF8STRING
    s = 171.chr
    [s.ljust(3), s.rjust(3), s.center(4)].each do |padded|
      assert_equal Encoding::BINARY, padded.encoding
      assert_true padded.valid_encoding?
    end
    assert_equal [171, 32, 32], s.ljust(3).bytes
    assert_equal [32, 32, 171], s.rjust(3).bytes
    assert_equal [32, 171, 32, 32], s.center(4).bytes
    # an ASCII receiver read as bytes keeps the byte reading too
    assert_equal Encoding::BINARY, "abc".b.ljust(5).encoding
    assert_equal Encoding::BINARY, "abc".b.rjust(5).encoding
    assert_equal Encoding::BINARY, "abc".b.center(5).encoding
    # a byte-read pad above ASCII marks a plain receiver's result
    assert_equal Encoding::BINARY, "ab".center(4, 255.chr).encoding
    # a byte-read pad of ASCII bytes moves nothing
    assert_equal Encoding::UTF_8, "ab".center(4, "-".b).encoding
    assert_equal Encoding::UTF_8, "あ".center(3).encoding
  end
end

assert('a byte-read string converted case') do
  # Bytes read as bytes spell no characters, so a case conversion has nothing
  # above ASCII to map and hands back the bytes it was given, still read as
  # bytes. The same bytes read as UTF-8 spell "Ä", which maps where the
  # build holds a table for it; where case follows ASCII there is nothing to
  # map and the two readings answer alike.
  if UTF8STRING
    s = "\xC3\x84B".b
    assert_equal [195, 132, 98], s.downcase.bytes
    assert_equal [195, 132, 66], s.upcase.bytes
    assert_equal [195, 132, 98], s.capitalize.bytes
    assert_equal Encoding::BINARY, s.downcase.encoding
    if UNICODECASE
      assert_equal [195, 164, 98], "\xC3\x84B".downcase.bytes
    else
      assert_equal [195, 132, 98], "\xC3\x84B".downcase.bytes
    end
  end
end

assert('a byte-read string cut in three') do
  # `partition` and `rpartition` cut their pieces out of the receiver's bytes,
  # so the head and the tail are read the way the receiver was. The middle
  # piece is the separator that was handed in and is read the way that was;
  # where no separator is found there is none to hand back, and the empty
  # pieces stand for places in the receiver instead.
  if UTF8STRING
    b = "a\xABb".b
    bin = Encoding::BINARY
    utf = Encoding::UTF_8
    assert_equal ["", "a", "\xABb"], b.partition("a")
    assert_equal ["a\xAB", "b", ""], b.rpartition("b")
    assert_equal [bin, utf, bin], b.partition("a").map { |piece| piece.encoding }
    assert_equal [bin, utf, bin], b.rpartition("b").map { |piece| piece.encoding }
    assert_true b.partition("a")[2].valid_encoding?
    # where the separator is found nowhere, the two empty pieces stand for
    # places in the receiver and are read the way it is
    assert_equal [bin, bin, bin], b.partition("x").map { |piece| piece.encoding }
    assert_equal [bin, bin, bin], b.rpartition("x").map { |piece| piece.encoding }
    # an empty separator cuts nothing off either end
    assert_equal [bin, utf, bin], b.partition("").map { |piece| piece.encoding }
    assert_equal [bin, utf, bin], b.rpartition("").map { |piece| piece.encoding }
    # a separator read as bytes hands its own reading to the middle piece alone
    assert_equal [utf, bin, utf], "aあb".partition("a".b).map { |piece| piece.encoding }
    assert_equal [utf, bin, utf], "aあb".rpartition("b".b).map { |piece| piece.encoding }
    # a receiver read as UTF-8 goes on being read that way
    assert_equal [utf, utf, utf], "あい".partition("あ").map { |piece| piece.encoding }
  end
end

assert('minrepro') do
  [->(s) { s << "z" }].each do |op|
    ["abc", "a" * 100, "", "a" * 31, "a" * 32].each do |base|
      warm = base.dup
      warm.length
      cold = base.dup
      op.call(warm)
      op.call(cold)
      assert_equal cold.chars.size, warm.chars.size
    end
  end
end
