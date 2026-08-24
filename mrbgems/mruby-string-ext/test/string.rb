##
# String(Ext) Test

UTF8STRING = __ENCODING__ == "UTF-8"
UNICODECASE = "\u00C4".downcase == "\u00E4"
# Which characters above ASCII are letters and digits is a table compiled under
# the pair the case tables are, MRB_UTF8_STRING without MRB_USE_ASCII_CTYPE, so
# what answers for the one answers for the other.
UNICODEALNUM = UNICODECASE

def assert_upto(exp, receiver, *args)
  act = []
  receiver.upto(*args) { |v| act << v }
  assert_equal exp, act
end

assert('String#dump') do
  assert_equal("\"\\x00\"", "\0".dump)
  assert_equal("\"foo\"", "foo".dump)
  assert_equal('"\xE3\x82\x8B"', "る".dump)
  assert_nothing_raised { ("\1" * 100).dump }   # regress #1210
end

assert('String#inspect of a binary string escapes every byte') do
  # `inspect` passes a whole character through unescaped so it stays readable,
  # which a string holding no characters has nothing to gain from. `dump` on
  # the same string escaped every byte already, so the two agree there.
  assert_equal('"る"', "る".inspect)
  assert_equal('"\xE3\x82\x8B"', "る".b.inspect)
  assert_equal("る".b.dump, "る".b.inspect)
end if UTF8STRING

assert('String#inspect leaves a malformed sequence malformed') do
  # `inspect` remembers a string it walked without meeting a character of
  # several bytes, so that whatever reads it next may step by bytes. A byte
  # spelling no character is escaped one at a time as well, and was remembered
  # the same way, after which it came back as a character the string does not
  # hold.
  ["\x80", "\xE3\x81", "\xC0\xAF", "\xED\xA0\x80", "\xF4\x90\x80\x80",
   "a\x80b"].each do |str|
    s = str.dup
    s.inspect
    assert_raise(ArgumentError) { s.codepoints }
  end

  s = "\xED\xA0\x80"
  s.inspect
  assert_raise(ArgumentError) { s.ord }
  s = "\xED\xA0\x80"
  s.inspect
  assert_equal "\u{FFFD}" * 3, s.scrub

  # an ASCII string does hold one character per byte, and is still read so
  s = "abc"
  s.inspect
  assert_equal [97, 98, 99], s.codepoints
  assert_equal "abc", s.scrub
end if UTF8STRING

assert('String#strip') do
  s = "  abc  "
  assert_equal("abc", s.strip)
  assert_equal("  abc  ", s)
  assert_equal("", "".strip)
  assert_equal("", " \t\r\n\f\v".strip)
  assert_equal("a", "\0a\0".strip)
  assert_equal("a", "\0 a \0".strip)
  assert_equal("", "\0 \0".strip)
  assert_equal("abc", "abc".strip)
  assert_equal("abc", "  abc".strip)
  assert_equal("abc", "abc  ".strip)
end

assert('String#lstrip') do
  s = "  abc  "
  assert_equal("abc  ", s.lstrip)
  assert_equal("  abc  ", s)
  assert_equal("", "".lstrip)
  assert_equal("", " \t\r\n\f\v".lstrip)
  assert_equal("a\0", "\0a\0".lstrip)
  assert_equal("a", "\0\0a".lstrip)
  assert_equal("a \0", "\0 a \0".lstrip)
  assert_equal("", "\0 \0".lstrip)
  assert_equal("abc", "abc".lstrip)
  assert_equal("abc", "  abc".lstrip)
  assert_equal("abc  ", "abc  ".lstrip)
end

assert('String#rstrip') do
  s = "  abc  "
  assert_equal("  abc", s.rstrip)
  assert_equal("  abc  ", s)
  assert_equal("", "".rstrip)
  assert_equal("", " \t\r\n\f\v".rstrip)
  assert_equal("\0a", "\0a\0".rstrip)
  assert_equal("\0 a", "\0 a \0".rstrip)
  assert_equal("", "\0 \0".rstrip)
  assert_equal("abc", "abc".rstrip)
  assert_equal("  abc", "  abc".rstrip)
  assert_equal("abc", "abc  ".rstrip)
end

assert('String#strip!') do
  s = "  abc  "
  t = "abc"
  assert_equal("abc", s.strip!)
  assert_equal("abc", s)
  assert_nil(t.strip!)
  assert_equal("abc", t)
  u = "\0 a \0"
  assert_equal("a", u.strip!)
  assert_equal("a", u)
  v = "\0 \0"
  assert_equal("", v.strip!)
  assert_equal("", v)
end

assert('String#lstrip!') do
  s = "  abc  "
  t = "abc  "
  assert_equal("abc  ", s.lstrip!)
  assert_equal("abc  ", s)
  assert_nil(t.lstrip!)
  assert_equal("abc  ", t)
  u = "\0 a \0"
  assert_equal("a \0", u.lstrip!)
  assert_equal("a \0", u)
  v = "\0\0"
  assert_equal("", v.lstrip!)
  assert_equal("", v)
end

assert('String#rstrip!') do
  s = "  abc  "
  t = "  abc"
  assert_equal("  abc", s.rstrip!)
  assert_equal("  abc", s)
  assert_nil(t.rstrip!)
  assert_equal("  abc", t)
end

assert('String#strip! family on a shared buffer') do
  # A substring shares the parent's heap buffer, so the strip family must read
  # the buffer pointer after mrb_str_modify unshares it. Reading it before means
  # the copy and the terminator land in the parent's buffer instead.
  base = ".        abcdefghijklmnopqrstuvwxyz0123456789"
  view = base[1..-1]
  assert_equal("abcdefghijklmnopqrstuvwxyz0123456789", view.lstrip!)
  assert_equal(".        abcdefghijklmnopqrstuvwxyz0123456789", base)

  base = "abcdefghijklmnopqrstuvwxyz0123456789        ."
  view = base[0..-2]
  assert_equal("abcdefghijklmnopqrstuvwxyz0123456789", view.rstrip!)
  assert_equal("abcdefghijklmnopqrstuvwxyz0123456789        .", base)

  base = ".        abcdefghijklmnopqrstuvwxyz0123456789        ."
  view = base[1..-2]
  assert_equal("abcdefghijklmnopqrstuvwxyz0123456789", view.strip!)
  assert_equal(".        abcdefghijklmnopqrstuvwxyz0123456789        .", base)
end

assert('String#swapcase') do
  assert_equal "hELLO", "Hello".swapcase
  assert_equal "CyBeR_pUnK11", "cYbEr_PuNk11".swapcase
end

assert('String#swapcase!') do
  s = "Hello"
  t = s.clone
  t.swapcase!
  assert_equal s.swapcase, t
end

assert('String#swapcase - Unicode') do
  skip unless UNICODECASE
  assert_equal "äÖ", "Äö".swapcase
  # A character with a lower case swaps down, one without swaps up, so a
  # mapping that spells more than one character comes back here too.
  assert_equal "SSa", "ßA".swapcase
  assert_equal "FI", "ﬁ".swapcase
  assert_equal "I", "ı".swapcase
  # A title case character swaps to what neither of its cases spells: U+01C5
  # upper cases to U+01C4 and lower cases to U+01C6, and swaps to "dŽ".
  assert_equal "dŽ", "ǅ".swapcase
  assert_equal "Ǆ", "ǆ".swapcase
  assert_equal "ǆ", "Ǆ".swapcase
  # A script without case has nothing to swap.
  assert_equal "日本", "日本".swapcase
  assert_nil "日本".swapcase!
end

assert('String#swapcase - ASCII only') do
  skip if UNICODECASE
  # Where case follows ASCII, a character above it has no case to swap and
  # stands between the ASCII that does.
  assert_equal "abÄCD", "ABÄcd".swapcase
  assert_nil "Ä".swapcase!
end

assert('String#swapcase! - a frozen receiver') do
  skip unless UNICODECASE
  # The Unicode walk raises from inside str_modify_keep_cr(), the same site
  # String#downcase! raises from, before the ASCII loop str_swapcase_bang
  # otherwise runs.
  assert_raise(FrozenError) { "Ä".freeze.swapcase! }
end

assert('String#concat') do
  assert_equal "Hello World!", "Hello " << "World" << 33
  assert_equal "Hello World!", "Hello ".concat("World").concat(33)
  assert_raise(TypeError) { "".concat(Object.new) }

  if UTF8STRING
    assert_equal "H«", "H" << 0xab
    assert_equal "Hは", "H" << 12399
  else
    assert_equal "H\xab", "H" << 0xab
    assert_raise(RangeError) { "H" << 12399 }
  end
end

assert('String#concat - a frozen receiver with nothing to append') do
  # An append of no bytes writes nothing and used to return before reaching
  # the frozen check, which sits with the write.
  assert_raise(FrozenError) { "abc".freeze << "" }
  assert_raise(FrozenError) { "abc".freeze.concat("") }
  assert_raise(FrozenError) { "".freeze << "" }
  assert_raise(FrozenError) { "abc".freeze.append_as_bytes("") }
  # An append that has bytes to add answers the same way.
  assert_raise(FrozenError) { "abc".freeze << "d" }
end

assert('String#prepend - a frozen receiver with nothing to prepend') do
  assert_raise(FrozenError) { "abc".freeze.prepend() }
  assert_raise(FrozenError) { "abc".freeze.prepend("") }
  assert_raise(FrozenError) { "abc".freeze.prepend("d") }
end

assert('String#concat on a shared buffer') do
  # An append to a string that shares its buffer writes into the spare
  # capacity above what every other sharer can see, and each sharer has to
  # keep the bytes it owns. Every string below is grown by appending before
  # it is shared, since a string that was never appended to has no spare
  # capacity and every append to it copies the buffer instead.

  # The parent appends in place; an interior slice must not see it.
  a = "a" * 100
  a << "z" * 100
  mid = a[0, 100]
  a << "q"
  assert_equal "a" * 100 + "z" * 100 + "q", a
  assert_equal "a" * 100, mid
  mid << "r"
  assert_equal "a" * 100 + "r", mid
  assert_equal "a" * 100 + "z" * 100 + "q", a

  # Two strings that end at the same offset: the first append claims the
  # bytes, and the other one has to take a copy of the buffer.
  b = "b" * 100
  b << "y" * 100
  c = b.dup
  b << "1"
  c << "2"
  assert_equal "b" * 100 + "y" * 100 + "1", b
  assert_equal "b" * 100 + "y" * 100 + "2", c

  # The same in the other order.
  d = "d" * 100
  d << "w" * 100
  e = d.dup
  e << "1"
  d << "2"
  assert_equal "d" * 100 + "w" * 100 + "1", e
  assert_equal "d" * 100 + "w" * 100 + "2", d

  # A tail slice ends where its parent does, so the same rule applies.
  f = "f" * 100
  f << "v" * 100
  g = f[170, 30]
  g << "1"
  f << "2"
  assert_equal "v" * 30 + "1", g
  assert_equal "f" * 100 + "v" * 100 + "2", f

  # Appending and slicing in a loop, the shape the copy made quadratic.
  h = ""
  50.times { h << "0123456789012345678901234567890123456789"; h[0, 30] }
  assert_equal 2000, h.length
  assert_equal "0123456789012345678901234567890123456789", h[-40, 40]

  # Appending a slice of a buffer to the string it was taken from.
  i = "i" * 100
  i << "j" * 100
  k = i[0, 40]
  i << k
  assert_equal "i" * 100 + "j" * 100 + "i" * 40, i
  assert_equal "i" * 40, k

  # A frozen sharer still raises, and does not stop the others.
  l = "l" * 100
  l << "m" * 100
  n = l.dup
  n.freeze
  assert_raise(FrozenError) { n << "x" }
  l << "y"
  assert_equal "l" * 100 + "m" * 100 + "y", l
  assert_equal "l" * 100 + "m" * 100, n
end

assert('String growth on a shared buffer') do
  # An append writes only above the length every other sharer of the buffer
  # can see, which is what lets `String#<<` above stay in the buffer. Growing
  # a string any other way has no such guarantee, and a growth that kept the
  # buffer shared would be seen through the other string. Every string here is
  # appended to before it is shared, since one with no spare capacity has
  # nothing to be grown into in place anyway.

  # `insert` at the front memmoves from offset 0, over the whole slice.
  a = "a" * 100
  a << "z" * 100
  a_slice = a[0, 150]
  a.insert(0, "1234")
  assert_equal "1234" + "a" * 100 + "z" * 100, a
  assert_equal "a" * 100 + "z" * 50, a_slice

  # `prepend`, the same shape as `insert` at 0.
  c = "c" * 100
  c << "x" * 100
  c_slice = c[0, 150]
  c.prepend("1234")
  assert_equal "1234" + "c" * 100 + "x" * 100, c
  assert_equal "c" * 100 + "x" * 50, c_slice

  # `insert` at the end appends, and an append writes only above what the
  # slice reads, so it stays in the buffer instead of taking a copy of it.
  b = "b" * 100
  b << "y" * 100
  b_slice = b[0, 150]
  b.insert(-1, "1234")
  assert_equal "b" * 100 + "y" * 100 + "1234", b
  assert_equal "b" * 100 + "y" * 50, b_slice

  # The loop that made growth at the end quadratic: the slice shares the
  # buffer again on every turn, and the append past it stays in place.
  f = ""
  50.times { f.insert(-1, "0123456789012345678901234567890123456789"); f[0, 30] }
  assert_equal 2000, f.length
  assert_equal "0123456789012345678901234567890123456789", f[-40, 40]

  # The rest are contract rather than detection: what they write happens to
  # land above the slice, or they take the buffer for reasons of their own.

  # `succ!` writes a terminator over the string before it grows, so it has to
  # hold the buffer by then whatever the growth does.
  d = "z" * 100
  d << "z" * 100
  d_slice = d[0, 150]
  d.succ!
  assert_equal "a" * 201, d
  assert_equal "z" * 150, d_slice

  # The sharer is the one that grows, ending below what its parent holds.
  e = "e" * 100
  e << "w" * 100
  e_slice = e[0, 150]
  e_slice.insert(0, "1234")
  assert_equal "1234" + "e" * 100 + "w" * 50, e_slice
  assert_equal "e" * 100 + "w" * 100, e
end

assert('String#casecmp') do
  assert_equal 1, "abcdef".casecmp("abcde")
  assert_equal 0, "aBcDeF".casecmp("abcdef")
  assert_equal(-1, "abcdef".casecmp("abcdefg"))
  assert_equal 0, "abcdef".casecmp("ABCDEF")
  # A byte of 0x80 or above orders above every ASCII one, which is where
  # `String#<=>` puts it as well.
  assert_equal 1, "\xC3".casecmp("a")
  assert_equal 1, ("\xC3" <=> "a")
  assert_equal(-1, "a".casecmp("\xC3"))
end

assert('String#casecmp?') do
  assert_true "aBcDeF".casecmp?("abcdef")
  assert_false "abcdef".casecmp?("abcde")
  assert_nil "abcdef".casecmp?(1)
end

assert('String#casecmp? - Unicode') do
  skip unless UNICODECASE
  # `casecmp` orders strings by ASCII case alone, which is CRuby's answer
  # there too; `casecmp?` folds instead, so it sees past the case.
  assert_equal 1, "ä".casecmp("Ä")
  assert_true "ä".casecmp?("Ä")
  # A folding can spell a character as several, which is what makes this
  # wider than comparing one character against one.
  assert_true "ß".casecmp?("ss")
  assert_true "ß".casecmp?("SS")
  # Only one side has to hold a character above ASCII for both to be folded,
  # and the other side is folded whether or not a walk over it has already
  # settled what it holds.
  ss = "SS"
  ss.length
  assert_true "ß".casecmp?(ss)
  assert_true ss.casecmp?("ß")
  assert_true "ﬁ".casecmp?("fi")
  # U+212A folds to "k", so the two spell the same string folded.
  assert_true "\u{212a}".casecmp?("k")
  # U+0130 folds to "i" plus U+0307, which "i" alone does not match.
  assert_false "İ".casecmp?("i")
  assert_false "日本".casecmp?("日")
  assert_true "日本".casecmp?("日本")
  # Bytes that spell no character have no folding, so the comparison refuses
  # them; `casecmp` orders the same bytes without asking what they spell.
  assert_raise(ArgumentError) { "\xC3ABC".casecmp?("a") }
  assert_equal 0, "\xC3ABC".casecmp("\xC3abc")
  assert_raise(ArgumentError) { "\xC3ABC".swapcase }
end

assert('String#casecmp? - ASCII only') do
  skip if UNICODECASE
  # Narrowed to ASCII, folding sees no further than `casecmp` does, so two
  # spellings that differ above ASCII stay apart however they would fold.
  assert_equal 1, "ä".casecmp("Ä")
  assert_false "ä".casecmp?("Ä")
  assert_false "ß".casecmp?("ss")
  assert_false "ﬁ".casecmp?("fi")
  # What is left of the folding is the ASCII half, and a folding that reads
  # nothing above ASCII has no bytes it must refuse.
  assert_true "äB".casecmp?("äb")
  assert_false "\xC3ABC".casecmp?("a")
  assert_equal 0, "\xC3ABC".casecmp("\xC3abc")
end

assert('String#count') do
  s = "abccdeff123"
  assert_equal 0, s.count("")
  assert_equal 1, s.count("a")
  assert_equal 2, s.count("ab")
  assert_equal 9, s.count("^c")
  assert_equal 8, s.count("a-z")
  assert_equal 4, s.count("a0-9")
end

assert('String#tr') do
  assert_equal "ABC", "abc".tr('a-z', 'A-Z')
  assert_equal "hippo", "hello".tr('el', 'ip')
  assert_equal "Ruby", "Lisp".tr("Lisp", "Ruby")
  assert_equal "*e**o", "hello".tr('^aeiou', '*')
  assert_equal "heo", "hello".tr('l', '')
end

assert('String#tr!') do
  s = "abcdefghijklmnopqR"
  assert_equal "ab12222hijklmnopqR", s.tr!("cdefg", "12")
  assert_equal "ab12222hijklmnopqR", s
end

assert('String#tr_s') do
  assert_equal "hero", "hello".tr_s('l', 'r')
  assert_equal "h*o", "hello".tr_s('el', '*')
  assert_equal "hhxo", "hello".tr_s('el', 'hx')
end

assert('String#tr_s!') do
  s = "hello"
  assert_equal "hero", s.tr_s!('l', 'r')
  assert_equal "hero", s
  assert_nil s.tr_s!('l', 'r')
end

assert('String#squeeze') do
  assert_equal "yelow mon", "yellow moon".squeeze
  assert_equal " now is the", "  now   is  the".squeeze(" ")
  assert_equal "puters shot balls", "putters shoot balls".squeeze("m-z")
end

assert('String#squeeze!') do
  s = "  now   is  the"
  assert_equal " now is the", s.squeeze!(" ")
  assert_equal " now is the", s
end

assert('String#delete') do
  assert_equal "he", "hello".delete("lo")
  assert_equal "hll", "hello".delete("aeiou")
  assert_equal "ll", "hello".delete("^l")
  assert_equal "ho", "hello".delete("ej-m")
end

assert('String#delete!') do
  s = "hello"
  assert_equal "he", s.delete!("lo")
  assert_equal "he", s
  assert_nil s.delete!("lz")
end

assert('String#start_with?') do
  assert_true "hello".start_with?("heaven", "hell")
  assert_true !"hello".start_with?("heaven", "paradise")
  assert_true !"h".start_with?("heaven", "hell")
  assert_raise TypeError do "hello".start_with?(true) end
end

assert('String#end_with?') do
  assert_true "string".end_with?("ing", "mng")
  assert_true !"string".end_with?("str", "tri")
  assert_true !"ng".end_with?("ing", "mng")
  assert_raise TypeError do "hello".end_with?(true) end
end

assert('String#partition') do
  assert_equal ["a", "x", "axa"], "axaxa".partition("x")
  assert_equal ["aaaaa", "", ""], "aaaaa".partition("x")
  assert_equal ["", "", "aaaaa"], "aaaaa".partition("")
  assert_equal ["", "a", "aaaa"], "aaaaa".partition("a")
  assert_equal ["aaaa", "b", ""], "aaaab".partition("b")
  assert_equal ["", "b", "aaaa"], "baaaa".partition("b")
  assert_equal ["", "", ""],      "".partition("a")
  assert_equal ["hello", " ", "world"], "hello world".partition(" ")
  assert_equal ["hell", "o", " world"], "hello world".partition("o")
  assert_equal ["hello world", "", ""], "hello world".partition("x")
end

assert('String#rpartition') do
  assert_equal ["axa", "x", "a"], "axaxa".rpartition("x")
  assert_equal ["", "", "aaaaa"], "aaaaa".rpartition("x")
  assert_equal ["aaaaa", "", ""], "aaaaa".rpartition("")
  assert_equal ["aaaa", "a", ""], "aaaaa".rpartition("a")
  assert_equal ["aaaa", "b", ""], "aaaab".rpartition("b")
  assert_equal ["", "b", "aaaa"], "baaaa".rpartition("b")
  assert_equal ["", "", ""],      "".rpartition("a")
  assert_equal ["hello", " ", "world"], "hello world".rpartition(" ")
  assert_equal ["hello w", "o", "rld"], "hello world".rpartition("o")
  assert_equal ["", "", "hello world"], "hello world".rpartition("x")
end

assert('String#hex') do
  assert_equal 16, "10".hex
  assert_equal 255, "ff".hex
  assert_equal 16, "0x10".hex
  assert_equal (-16), "-0x10".hex
  assert_equal 0, "xyz".hex
  assert_equal 16, "10z".hex
  assert_equal 16, "1_0".hex
  assert_equal 0, "".hex
end

assert('String#oct') do
  assert_equal 8, "10".oct
  assert_equal 7, "7".oct
  assert_equal 0, "8".oct
  assert_equal 0, "9".oct
  assert_equal 0, "xyz".oct
  assert_equal 8, "10z".oct
  assert_equal 8, "1_0".oct
  assert_equal 8, "010".oct
  assert_equal (-8), "-10".oct
end

assert('String#lines') do
  assert_equal ["Hel\n", "lo\n", "World!"], "Hel\nlo\nWorld!".lines
  assert_equal ["Hel\n", "lo\n", "World!\n"], "Hel\nlo\nWorld!\n".lines
  assert_equal ["\n", "\n", "\n"], "\n\n\n".lines
  assert_equal [], "".lines
end

assert('String#clear') do
  # embed string
  s = "foo"
  assert_equal("", s.clear)
  assert_equal("", s)

  # not embed string and not shared string
  s = "foo" * 100
  a = s
  assert_equal("", s.clear)
  assert_equal("", s)
  assert_equal("", a)

  # shared string
  s = "foo" * 100
  a = s[10, 90]                # create shared string
  assert_equal("", s.clear)    # clear
  assert_equal("", s)          # s is cleared
  assert_not_equal("", a)      # a should not be affected
end

assert('String#slice!') do
  a = "AooBar"
  b = a.dup
  assert_equal "A", a.slice!(0)
  assert_equal "AooBar", b

  a = "FooBar"
  assert_equal "r", a.slice!(-1)
  assert_equal "FooBa", a

  a = "FooBar"
  assert_nil a.slice!(6)
  assert_nil a.slice!(-7)
  assert_equal "FooBar", a

  a = "FooBar"
  assert_equal "Foo", a.slice!(0, 3)
  assert_equal "Bar", a

  a = "FooBar"
  assert_equal "Bar", a.slice!(-3, 3)
  assert_equal "Foo", a

  a = "FooBar"
  assert_equal "", a.slice!(6, 2)
  assert_equal "FooBar", a

  a = "FooBar"
  assert_nil a.slice!(-7,10)
  assert_equal "FooBar", a

  a = "FooBar"
  assert_equal "Foo", a.slice!(0..2)
  assert_equal "Bar", a

  a = "FooBar"
  assert_equal "Bar", a.slice!(-3..-1)
  assert_equal "Foo", a

  a = "FooBar"
  assert_equal "", a.slice!(6..2)
  assert_equal "FooBar", a

  a = "FooBar"
  assert_nil a.slice!(-10..-7)
  assert_equal "FooBar", a

  a = "FooBar"
  assert_equal "Foo", a.slice!("Foo")
  assert_equal "Bar", a

  a = "FooBar"
  assert_nil a.slice!("xyzzy")
  assert_equal "FooBar", a

  assert_raise(ArgumentError) { "foo".slice! }
end

assert('String#slice! with multibyte characters') do
  a = "あいうえお"
  assert_equal "えお", a.slice!(3, 2)
  assert_equal "あいう", a

  a = "あいう"
  assert_equal "いう", a.slice!(1..2)
  assert_equal "あ", a

  a = "あいう"
  assert_equal "う", a.slice!(-1)
  assert_equal "あい", a

  a = "aあいb"
  assert_equal "あい", a.slice!(1, 2)
  assert_equal "ab", a

  a = "あい"
  assert_equal "あい", a.slice!(0, 2)
  assert_equal "", a

  a = "あい"
  assert_equal "", a.slice!(2, 1)
  assert_equal "あい", a
end if UTF8STRING

assert('String#slice! with a multibyte match') do
  a = "あいう"
  assert_equal "い", a.slice!("い")
  assert_equal "あう", a

  a = "あいう"
  assert_equal "う", a.slice!("う")
  assert_equal "あい", a

  a = "あいう"
  assert_nil a.slice!("え")
  assert_equal "あいう", a

  # the search runs over bytes, so a match starting inside a character is
  # not a match
  a = "あ"
  assert_nil a.slice!("\x81\x82")
  assert_equal "あ", a
end if UTF8STRING

assert('String#succ') do
  assert_equal "", "".succ
  assert_equal "1", "0".succ
  assert_equal "10", "9".succ
  assert_equal "01", "00".succ
  assert_equal "a1", "a0".succ
  assert_equal "A1", "A0".succ
  assert_equal "10", "09".succ
  assert_equal "b0", "a9".succ
  assert_equal "B0", "A9".succ

  assert_equal "b", "a".succ
  assert_equal "aa", "z".succ
  assert_equal "ab", "aa".succ
  assert_equal "Ab", "Aa".succ
  assert_equal "0b", "0a".succ
  assert_equal "ba", "az".succ
  assert_equal "Ba", "Az".succ
  assert_equal "1a", "0z".succ

  assert_equal "B", "A".succ
  assert_equal "AA", "Z".succ
  assert_equal "AB", "AA".succ
  assert_equal "aB", "aA".succ
  assert_equal "0B", "0A".succ
  assert_equal "BA", "AZ".succ
  assert_equal "bA", "aZ".succ
  assert_equal "1A", "0Z".succ

  assert_equal ".", "-".succ
  assert_equal "-b", "-a".succ
  assert_equal "-aa", "-z".succ
  assert_equal "-a-b-", "-a-a-".succ
  assert_equal "-b-", "-a-".succ
  assert_equal "-aa-", "-z-".succ
  assert_equal "あb", "あa".succ
  assert_equal "あba", "あaz".succ

  # a wrap carries across what is not alphanumeric, letter to letter and
  # digit to digit, but not from a letter into a digit or the other way
  assert_equal "b-a", "a-z".succ
  assert_equal "2.0", "1.9".succ
  assert_equal "-10", "-9".succ
  assert_equal "1-aa", "1-z".succ
  assert_equal "a-10", "a-9".succ
  assert_equal "9-aa", "9-z".succ
  assert_equal "AAa", "Zz".succ
  assert_equal "aaa00", "zz99".succ
  assert_equal "**+", "***".succ

  a = ""; a.succ!
  assert_equal "", a
  assert_raise(FrozenError) { "".freeze.succ! }
  assert_raise(FrozenError) { "a".freeze.succ! }
  a = "0"; a.succ!
  assert_equal "1", a
  a = "9"; a.succ!
  assert_equal "10", a
  a = "00"; a.succ!
  assert_equal "01", a
  a = "a0"; a.succ!
  assert_equal "a1", a
  a = "A0"; a.succ!
  assert_equal "A1", a
  a = "09"; a.succ!
  assert_equal "10", a
  a = "a9"; a.succ!
  assert_equal "b0", a
  a = "A9"; a.succ!
  assert_equal "B0", a

  a = "a"; a.succ!
  assert_equal "b", a
  a = "z"; a.succ!
  assert_equal "aa", a
  a = "aa"; a.succ!
  assert_equal "ab", a
  a = "Aa"; a.succ!
  assert_equal "Ab", a
  a = "0a"; a.succ!
  assert_equal "0b", a
  a = "az"; a.succ!
  assert_equal "ba", a
  a = "Az"; a.succ!
  assert_equal "Ba", a
  a = "0z"; a.succ!
  assert_equal "1a", a

  a = "A"; a.succ!
  assert_equal "B", a
  a = "Z"; a.succ!
  assert_equal "AA", a
  a = "AA"; a.succ!
  assert_equal "AB", a
  a = "aA"; a.succ!
  assert_equal "aB", a
  a = "0A"; a.succ!
  assert_equal "0B", a
  a = "AZ"; a.succ!
  assert_equal "BA", a
  a = "aZ"; a.succ!
  assert_equal "bA", a
  a = "0Z"; a.succ!
  assert_equal "1A", a

  a = "-"; a.succ!
  assert_equal ".", a
  a = "-a"; a.succ!
  assert_equal "-b", a
  a = "-z"; a.succ!
  assert_equal "-aa", a
  a = "-a-a-"; a.succ!
  assert_equal "-a-b-", a
  a = "-a-"; a.succ!
  assert_equal "-b-", a
  a = "-z-"; a.succ!
  assert_equal "-aa-", a
  a = "あb"; a.succ!
  assert_equal "あc", a
  a = "あaz"; a.succ!
  assert_equal "あba", a
end

assert('String#next') do
  assert_equal "01", "00".next

  a = "00"; a.next!
  assert_equal "01", a
end

assert('String#succ steps a string with no alphanumeric by character') do
  # A string read as bytes steps its last byte, and 0xff wraps to 0x00 and
  # carries into the byte before it; when every byte wraps, "\x01" goes in
  # front. Whatever the build reads its strings as, a byte-read string steps
  # this way.
  assert_equal "\x01\x00".b, "\xff".b.succ
  assert_equal "\x01\x00\x00".b, "\xff\xff".b.succ
  assert_equal "\x80".b, "\x7f".b.succ
  assert_equal "\x01".b, "\x00".b.succ
  assert_equal "b\xff".b, "a\xff".b.succ
  assert_equal "aa\xff".b, "z\xff".b.succ
  assert_equal "\xC3\xC0".b, "ÿ".b.succ
  a = "\xff\xff".b; a.succ!
  assert_equal "\x01\x00\x00".b, a

  if UTF8STRING
    # A UTF-8 string steps its last character to the next code point of the
    # same byte length, and wraps to the first of that length where the next
    # would take one more; a run of bytes that spells no character is left
    # alone.
    assert_equal "Ā", "ÿ".succ
    assert_equal "ぃ", "あ".succ
    assert_equal "\u{81}", "\u{80}".succ
    assert_equal "\u{E000}", "\u{D7FF}".succ
    assert_equal "b\u{D7FF}", "a\u{D7FF}".succ
    assert_equal "\x01\x00", "\x7f".succ
    assert_equal "\x01\x00\x00", "\x7f\x7f".succ
    assert_equal "\x01\u{80}", "\u{7FF}".succ
    assert_equal "b\u{7FF}", "a\u{7FF}".succ
    assert_equal "\x01\u{800}", "\u{FFFF}".succ
    assert_equal "\x01\u{10000}", "\u{10FFFF}".succ
    assert_equal "b\u{10FFFF}", "a\u{10FFFF}".succ
    assert_equal "\x01\xff", "\xff".succ
    assert_equal "\x01\xff\xff", "\xff\xff".succ
    assert_equal "b\xff", "a\xff".succ
    assert_equal "aa\xff", "z\xff".succ
    assert_equal ["ÿ", "Ā", "ā"], ("ÿ".."ā").to_a
    a = "ÿ"; a.succ!
    assert_equal "Ā", a
  else
    # every byte is a character, so this build steps every string by bytes
    assert_equal "\x01\x00", "\xff".succ
    assert_equal "\x01\x00\x00", "\xff\xff".succ
    assert_equal "\x80", "\x7f".succ
    assert_equal "\xC3\xC0", "ÿ".succ
    a = "\xff"; a.succ!
    assert_equal "\x01\x00", a
  end
end

assert('String#succ steps a letter or a digit above ASCII') do
  # Which characters those are is the table generated from the Unicode
  # character database, the properties CRuby asks its encoding for: a letter
  # steps to the next letter and a digit to the next digit.
  assert_equal "\u0100", "\u00FF".succ
  assert_equal "a\u0100", "a\u00FF".succ
  assert_equal "\u3043", "\u3042".succ
  assert_equal "\u3094", "\u3093".succ
  assert_equal "\uFF11", "\uFF10".succ
  assert_equal "\uFF22", "\uFF21".succ

  # over one code point that is not a letter, as CRuby steps U+03A1 to U+03A3
  # over the unassigned U+03A2
  assert_equal "\u03A3", "\u03A1".succ

  # the end of a run wraps to the start of that run and carries a character of
  # it, where the end of the ASCII letters carries an "a"
  assert_equal "\u05D0\u05D0", "\u05EA".succ
  assert_equal "b\u05D0", "a\u05EA".succ
  assert_equal "-\u05D0\u05D0", "-\u05EA".succ
  assert_equal "\u05D0\u05D0\u05D0", "\u05EA\u05EA".succ
  assert_equal "\uFF41\uFF41", "\uFF5A".succ
  assert_equal "\uFF41\uFF41\uFF41", "\uFF5A\uFF5A".succ

  # a digit run carries the digit after its first, where "9" carries "1"
  assert_equal "\u0661\u0660", "\u0669".succ
  assert_equal "b\u0660", "a\u0669".succ
  assert_equal "\u0661\u0660\u0660", "\u0669\u0669".succ
  assert_equal "\uFF11\uFF10", "\uFF19".succ
  assert_equal "\u{1D7CF}\u{1D7CE}", "\u{1D7FF}".succ

  # the carry crosses what is not alphanumeric and lands on the alphanumeric
  # before it, whichever of the two is the wider character
  assert_equal "b\uFF41", "a\uFF5A".succ
  assert_equal "10\u05D0", "9\u05EA".succ

  # a letter alone in its run has nowhere to wrap to, so it is not one this
  # walk steps: U+00A9 and U+00AB are on either side of U+00AA
  assert_equal "\u00AB", "\u00AA".succ
  assert_equal "b\u00AA", "a\u00AA".succ

  # so above ASCII it is the letter that steps where an ASCII one is beside it
  assert_equal "a\u0100", "a\u00FF".succ
  assert_equal "1\u3043", "1\u3042".succ
  assert_equal [0x61, 0xC4, 0x80], "a\u00FF".succ.bytes
end if UNICODEALNUM

assert('String#succ has no letter or digit above ASCII without the table') do
  # A build reading its strings as bytes, and one narrowed by
  # MRB_USE_ASCII_CTYPE, carries no table: nothing above ASCII is a letter or a
  # digit there, and the last character steps as a character instead, which for
  # these is the same answer as stepping the last byte.
  assert_equal "\u05EB", "\u05EA".succ
  assert_equal "\u066A", "\u0669".succ
  assert_equal "\uFF5B", "\uFF5A".succ
  # an ASCII letter or digit before it is what steps instead
  assert_equal "b\u05EA", "a\u05EA".succ
  assert_equal "b\uFF5A", "a\uFF5A".succ
  assert_equal "b\u00FF", "a\u00FF".succ
  assert_equal "2\u3042", "1\u3042".succ
end unless UNICODEALNUM

assert('String#insert') do
  assert_equal "Xabcd", "abcd".insert(0, 'X')
  assert_equal "abcXd", "abcd".insert(3, 'X')
  assert_equal "abcdX", "abcd".insert(4, 'X')
  assert_equal "abXcd", "abcd".insert(-3, 'X')
  assert_equal "abcdX", "abcd".insert(-1, 'X')
  assert_raise(IndexError) { "abcd".insert(5, 'X') }
  assert_raise(IndexError) { "abcd".insert(-6, 'X') }

  a = "abcd"
  a.insert(0, 'X')
  assert_equal "Xabcd", a
end

assert('String#prepend') do
  # Basic prepend test
  a = "world"
  assert_equal "hello world", a.prepend("hello ")
  assert_equal "hello world", a

  # Multiple arguments test
  b = "world"
  assert_equal "hello beautiful world", b.prepend("hello ", "beautiful ")
  assert_equal "hello beautiful world", b

  # Empty string test
  c = "test"
  assert_equal "test", c.prepend("")
  assert_equal "test", c

  # No arguments test
  d = "test"
  assert_equal "test", d.prepend()
  assert_equal "test", d

  # Prepend to empty string
  e = ""
  assert_equal "hello", e.prepend("hello")
  assert_equal "hello", e

  # Multiple empty strings
  f = "world"
  assert_equal "world", f.prepend("", "", "")
  assert_equal "world", f

  # Mixed empty and non-empty
  g = "world"
  assert_equal "hello world", g.prepend("", "hello ", "")
  assert_equal "hello world", g

  # Self-referencing arguments (GHSA-3hgj-g76g-878c)
  h = "A" * 100
  h.prepend(h, h)
  assert_equal 300, h.length
  assert_equal "A" * 300, h

  # Mixed self-reference and literal
  i = "AB"
  i.prepend("XYZ", i)
  assert_equal "XYZABAB", i

  j = "AB"
  j.prepend(j, "X", j)
  assert_equal "ABXABAB", j
end

assert('String#ljust') do
  assert_equal "hello", "hello".ljust(4)
  assert_equal "hello               ", "hello".ljust(20)
  assert_equal 20, "hello".ljust(20).length
  assert_equal "hello123412341234123", "hello".ljust(20, '1234')
  assert_equal "hello", "hello".ljust(-3)
end

assert('String#rjust') do
  assert_equal "hello", "hello".rjust(4)
  assert_equal "               hello", "hello".rjust(20)
  assert_equal 20, "hello".rjust(20).length
  assert_equal "123412341234123hello", "hello".rjust(20, '1234')
  assert_equal "hello", "hello".rjust(-3)
end

assert('String#center') do
  assert_equal "hello", "hello".center(4)
  assert_equal "       hello        ", "hello".center(20)
  assert_equal 20, "hello".center(20).length
  assert_equal "1231231hello12312312", "hello".center(20, '123')
  assert_equal "hello", "hello".center(-3)
end

if UTF8STRING
  assert('String#ljust with UTF8') do
    assert_equal "helloん              ", "helloん".ljust(20)
    assert_equal "helloó                            ", "helloó".ljust(34)
    assert_equal 34, "helloó".ljust(34).length
    assert_equal "helloんんんんんんんんんんんんんん", "hello".ljust(19, 'ん')
    assert_equal "helloんんんんんんんんんんんんんんん", "hello".ljust(20, 'ん')
  end

  assert('String#rjust with UTF8') do
    assert_equal "              helloん", "helloん".rjust(20)
    assert_equal "                            helloó", "helloó".rjust(34)
    # assert_equal 34, "helloó".rjust(34).length
    assert_equal "んんんんんんんんんんんんんんhello", "hello".rjust(19, 'ん')
    assert_equal "んんんんんんんんんんんんんんんhello", "hello".rjust(20, 'ん')
  end

  assert('UTF8 byte counting') do
    ret = '                                  '
    ret[-6..-1] = "helloó"
    assert_equal 34, ret.length
  end
end

assert('String#ljust should not change string') do
  a = "hello"
  a.ljust(20)
  assert_equal "hello", a
end

assert('String#rjust should not change string') do
  a = "hello"
  a.rjust(20)
  assert_equal "hello", a
end

assert('String#ljust should raise on zero width padding') do
  assert_raise(ArgumentError) { "foo".ljust(10, '') }
end

assert('String#rjust should raise on zero width padding') do
  assert_raise(ArgumentError) { "foo".rjust(10, '') }
end

assert('String#upto') do
  assert_upto %w(a8 a9 b0 b1 b2 b3 b4 b5 b6), "a8", "b6"
  assert_upto ["9", "10", "11"], "9", "11"
  assert_upto [], "25", "5"
  assert_upto ["07", "08", "09", "10", "11"], "07", "11"
  assert_upto ["9", ":", ";", "<", "=", ">", "?", "@", "A"], "9", "A"

  if UTF8STRING
    assert_upto %w(あ ぃ い ぅ う ぇ え ぉ お), "あ", "お"
  end

  a     = "aa"
  start = "aa"
  count = 0
  assert_equal("aa", a.upto("zz") {|s|
    assert_equal(start, s)
    start.succ!
    count += 1
  })
  assert_equal(676, count)

  a     = "a"
  start = "a"
  count = 0
  assert_equal("a", a.upto("a") {|s|
    assert_equal(start, s)
    start.succ!
    count += 1
  })
  assert_equal(1, count)

  a     = "a"
  start = "a"
  count = 0
  assert_equal("a", a.upto("b", true) {|s|
    assert_equal(start, s)
    start.succ!
    count += 1
  })
  assert_equal(1, count)

  a     = "0"
  start = "0"
  count = 0
  assert_equal("0", a.upto("0") {|s|
    assert_equal(start, s)
    start.succ!
    count += 1
  })
  assert_equal(1, count)

  a     = "0"
  start = "0"
  count = 0
  assert_equal("0", a.upto("-1") {|s|
    assert_equal(start, s)
    start.succ!
    count += 1
  })
  assert_equal(0, count)

  a     = "-1"
  start = "-1"
  count = 0
  assert_equal("-1", a.upto("-2") {|s|
    assert_equal(start, s)
    start.succ!
    count += 1
  })
  assert_equal(2, count)

  assert_raise(TypeError) { "a".upto(:c) {} }
end

assert('String#ord') do
  got = "hello!".split('').map {|x| x.ord}
  expect = [104, 101, 108, 108, 111, 33]
  unless UTF8STRING
    got << "\xff".ord
    expect << 0xff
  end
  assert_equal expect, got
end

assert('String#ord(UTF-8)') do
  got = "こんにちは世界!".split('').map {|x| x.ord}
  expect = [0x3053,0x3093,0x306b,0x3061,0x306f,0x4e16,0x754c,0x21]
  assert_equal expect, got
end if UTF8STRING

assert('String#ord(UTF-8) rejects ill-formed sequences', '#2708') do
  # overlong encodings (RFC 3629)
  assert_raise(ArgumentError) { "\xC0\x80".ord }       # 2-byte overlong NUL
  assert_raise(ArgumentError) { "\xE0\x80\x80".ord }   # 3-byte overlong NUL
  assert_raise(ArgumentError) { "\xF0\x80\x80\x80".ord } # 4-byte overlong NUL
  assert_raise(ArgumentError) { "\xE0\x9F\xBF".ord }   # overlong U+07FF as 3 bytes
  # UTF-16 surrogates encoded as UTF-8
  assert_raise(ArgumentError) { "\xED\xA0\x80".ord }   # U+D800
  assert_raise(ArgumentError) { "\xED\xBF\xBF".ord }   # U+DFFF
  # above U+10FFFF
  assert_raise(ArgumentError) { "\xF4\x90\x80\x80".ord } # U+110000
end if UTF8STRING

assert('String#chr') do
  assert_equal "a", "abcde".chr
  assert_equal "h", "hello!".chr
  assert_equal "", "".chr
end

assert('String#chr(UTF-8)') do
  assert_equal "こ", "こんにちは世界!".chr
end if UTF8STRING

assert('String#chars') do
  expect = ["h", "e", "l", "l", "o", "!"]
  assert_equal expect, "hello!".chars
  s = ""
  "hello!".chars do |x|
    s += x
  end
  assert_equal "hello!", s
end

assert('String#chars(UTF-8)') do
  expect = ['こ', 'ん', 'に', 'ち', 'は', '世', '界', '!']
  assert_equal expect, "こんにちは世界!".chars
  s = ""
  "こんにちは世界!".chars do |x|
    s += x
  end
  assert_equal "こんにちは世界!", s
end if UTF8STRING

assert('String#chars splits a malformed sequence by byte') do
  # A byte standing for no character is one position of its own, which is
  # what #length counts it as.
  assert_equal ["\xC0", "\x80"], "\xC0\x80".chars                          # overlong "/"
  assert_equal ["\xED", "\xA0", "\x80"], "\xED\xA0\x80".chars              # surrogate U+D800
  assert_equal ["\xF4", "\x90", "\x80", "\x80"], "\xF4\x90\x80\x80".chars  # > U+10FFFF
  assert_equal ["\xE3", "\x81"], "\xE3\x81".chars                          # truncated
  assert_equal ["a", "\x80", "b"], "a\x80b".chars                          # stray continuation
  assert_equal ["あ", "\xFE", "い"], "あ\xFEい".chars                      # sequence leads nothing

  ["\xC0\x80", "\xED\xA0\x80", "\xF4\x90\x80\x80", "\xE3\x81", "a\x80b",
   "あ\xFEい", "あいu", "hello"].each do |str|
    assert_equal str.length, str.chars.size
  end
end if UTF8STRING

assert('String#each_char') do
  chars = []
  "hello!".each_char do |x|
    chars << x
  end
  assert_equal ["h", "e", "l", "l", "o", "!"], chars
end

assert('String#each_char(UTF-8)') do
  chars = []
  "こんにちは世界!".each_char do |x|
    chars << x
  end
  assert_equal ["こ", "ん", "に", "ち", "は", "世", "界", "!"], chars
end if UTF8STRING

assert('String#chop! on a binary string removes one byte') do
  # `chop!` cuts at the last character, and a byte-indexed string ends in a
  # byte rather than in a character. Reading it as UTF-8 took the whole of a
  # multi-byte sequence off, or all of a string that held only one.
  s = "\u{1F600}".b   # F0 9F 98 80: four bytes, one character
  s.chop!
  assert_equal "\xF0\x9F\x98".b, s
  t = "a\u{1F600}".b
  t.chop!
  assert_equal "a\xF0\x9F\x98".b, t
  # a string read as UTF-8 still loses the whole character
  u = "\u{1F600}"
  u.chop!
  assert_equal "", u
  # and the \r\n pair is still taken together
  v = "a\r\n".b
  v.chop!
  assert_equal "a", v
end if UTF8STRING

assert('String#rindex on a binary string counts bytes') do
  # `rindex` reads a byte-indexed string as UTF-8 unless the single-byte flag
  # is already set, so it stepped over the bytes inside a multi-byte sequence
  # and moved a negative position by characters. `index` counts bytes there,
  # and the two have to meet.
  s = "aあb".b # "\x61\xe3\x81\x82\x62"
  assert_equal 2, s.rindex("\x81".b)
  assert_equal s.index("\x81".b), s.rindex("\x81".b)
  assert_equal 1, s.rindex("\xe3".b)
  assert_equal 4, s.rindex("b".b)
  assert_equal 2, s.rindex("\x81".b, -2)
  assert_equal 2, s.rindex("\x81".b, -3)
  assert_nil s.rindex("\x81".b, 1)
  assert_equal 4, s.rindex("b".b, -1)
  # the same answers once #length has set the single-byte flag
  assert_equal 5, s.length
  assert_equal 2, s.rindex("\x81".b)
  assert_equal 2, s.rindex("\x81".b, -2)
end if UTF8STRING

assert('a needle that spells no character is found nowhere') do
  # A search reads its needle as the encoding the needle is taken to be in,
  # and bytes that spell no character name nothing to look for. CRuby answers
  # the same way. A byte-indexed needle claims no encoding, so it is still
  # searched for byte by byte, which is how the byte cases below get asked.
  s = "aあb"                       # 61 E3 81 82 62
  assert_nil s.index("\x81")
  assert_nil s.rindex("\x81")
  assert_nil s.byteindex("\x81")
  assert_nil s.byterindex("\x81")
  assert_nil s["\x81"]
  assert_false s.include?("\x81")
  assert_false s.end_with?("\x82")
  assert_equal [s, "", ""], s.partition("\x81")
  assert_equal ["", "", s], s.rpartition("\x81")
  assert_equal s, s.chomp("\x82")
  t = s.dup
  assert_nil t.slice!("\x81")
  assert_equal s, t

  # byte-indexed, the same bytes are bytes and every search answers
  b = s.b
  assert_equal 2, b.index("\x81".b)
  assert_equal 2, b.rindex("\x81".b)
  assert_equal 2, b.byteindex("\x81".b)
  assert_equal 2, b.byterindex("\x81".b)
  assert_equal 1, b.byterindex("\xe3".b)
  assert_true b.include?("\x81".b)
  assert_equal 3, "あ\x80x".b.rindex("\x80".b)
  assert_equal 2, "\xC0\x80a".b.rindex("a".b)

  # a whole character is still found, and so is ASCII
  assert_equal 1, s.index("あ")
  assert_equal 2, s.index("b")
  assert_equal 4, s.byterindex("b")
end if UTF8STRING

assert('String#codepoints') do
  expect = [104, 101, 108, 108, 111, 33]
  assert_equal expect, "hello!".codepoints
  cp = []
  "hello!".codepoints do |x|
    cp << x
  end
  assert_equal expect, cp
end

assert('String#codepoints(UTF-8)') do
  expect = [12371, 12435, 12395, 12385, 12399, 19990, 30028, 33]
  assert_equal expect, "こんにちは世界!".codepoints
  cp = []
  "こんにちは世界!".codepoints do |x|
    cp << x
  end
  assert_equal expect, cp
end if UTF8STRING

assert('String#codepoints rejects malformed sequences') do
  assert_raise(ArgumentError) { "\x80".codepoints }             # stray continuation
  assert_raise(ArgumentError) { "\xE3\x81".codepoints }         # truncated
  assert_raise(ArgumentError) { "\xC0\xAF".codepoints }         # overlong "/"
  assert_raise(ArgumentError) { "\xED\xA0\x80".codepoints }     # surrogate U+D800
  assert_raise(ArgumentError) { "\xF4\x90\x80\x80".codepoints } # > U+10FFFF
  assert_raise(ArgumentError) { "\xF8\x88\x80\x80\x80".codepoints }
  # the walk keeps its place: characters after a 4-byte one still decode
  assert_equal [128169, 97], "\u{1F4A9}a".codepoints
  assert_raise(ArgumentError) { "\u{1F4A9}\xC0\xAF".codepoints }
end if UTF8STRING

assert('String#ord rejects malformed sequences') do
  assert_raise(ArgumentError) { "\x80".ord }
  assert_raise(ArgumentError) { "\xE3\x81".ord }
  assert_raise(ArgumentError) { "\xC0\xAF".ord }
  assert_raise(ArgumentError) { "\xED\xA0\x80".ord }
  assert_raise(ArgumentError) { "\xF4\x90\x80\x80".ord }
end if UTF8STRING

assert('a malformed sequence stays malformed once the string is counted') do
  # Counting characters remembers a string that holds one per byte, so that
  # whatever reads it next may step by bytes. Bytes that spell no character
  # count one per byte as well, and were remembered the same way, after which
  # they came back as the characters the string does not hold.
  counters = {
    "length"    => ->(s) { s.length },
    "chars"     => ->(s) { s.chars },
    "each_char" => ->(s) { s.each_char { |c| } },
    "[]"        => ->(s) { s[0] },
  }
  broken = ["\x80", "\xE3\x81", "\xC0\xAF", "\xED\xA0\x80", "\xF4\x90\x80\x80",
            "a\x80b", "あ\x80"]
  # #ord reads the first character, so it answers wherever that one is whole
  ord_answers = { "a\x80b" => 97, "あ\x80" => 12354 }

  counters.each do |name, count|
    broken.each do |str|
      s = str.dup
      count.call(s)
      assert_raise(ArgumentError, "#{str.inspect}.codepoints after #{name}") { s.codepoints }
    end
    broken.each do |str|
      s = str.dup
      count.call(s)
      want = ord_answers[str]
      if want
        assert_equal want, s.ord, "#{str.inspect}.ord after #{name}"
      else
        assert_raise(ArgumentError, "#{str.inspect}.ord after #{name}") { s.ord }
      end
    end

    # scrub has nothing to replace only where the bytes read as characters
    s = "\xED\xA0\x80"
    count.call(s)
    assert_equal "\u{FFFD}" * 3, s.scrub
    s = "\xED\xA0\x80"
    count.call(s)
    assert_equal "???", s.scrub("?")

    # an ASCII string does hold one character per byte, and is still read so
    s = "abc"
    count.call(s)
    assert_equal [97, 98, 99], s.codepoints
    assert_equal 97, s.ord
    assert_equal "abc", s.scrub
  end
end if UTF8STRING

assert('String#each_codepoint') do
  expect = [104, 101, 108, 108, 111, 33]
  cp = []
  "hello!".each_codepoint do |x|
    cp << x
  end
  assert_equal expect, cp
end

assert('String#each_codepoint(UTF-8)') do
  expect = [12371, 12435, 12395, 12385, 12399, 19990, 30028, 33]
  cp = []
  "こんにちは世界!".each_codepoint do |x|
    cp << x
  end
  assert_equal expect, cp
end if UTF8STRING

assert('String#delete_prefix') do
  assert_equal "llo", "hello".delete_prefix("he")
  assert_equal "hello", "hello".delete_prefix("llo")
  assert_equal "llo", "hello".delete_prefix!("he")
  assert_nil "hello".delete_prefix!("llo")
end

assert('String#delete_suffix') do
  assert_equal "he", "hello".delete_suffix("llo")
  assert_equal "hello", "hello".delete_suffix("he")
  assert_equal "he", "hello".delete_suffix!("llo")
  assert_nil "hello".delete_suffix!("he")
end

assert('String#+@') do
  a = +"abc"
  assert_false(a.frozen?)
  a = +(a.freeze)
  assert_false(a.frozen?)
end

assert('String#-@') do
  a = -"abc"
  assert_true(a.frozen?)
  a = -(a.freeze)
  assert_true(a.frozen?)
end

assert('String#scrub default replacement (U+FFFD)') do
  # scrub has UTF-8 semantics; on builds without MRB_UTF8_STRING it
  # degrades to a no-op (verified separately below).
  skip unless "あ".length == 1
  assert_equal "\u{FFFD}",       "\xE3\x81".scrub
  assert_equal "abc\u{FFFD}def", "abc\x80def".scrub
  # Unicode 3.9 gives one U+FFFD to each maximal subpart, not one to a run
  assert_equal "\u{FFFD}\u{FFFD}\u{FFFD}", "\x80\x81\x82".scrub
  assert_equal "",               "".scrub
  assert_equal "hello",          "hello".scrub          # already valid
  assert_equal "あい",   "あい".scrub   # already valid multibyte
end

assert('String#scrub rejects malformed sequences') do
  skip unless "あ".length == 1
  # overlong, UTF-16 surrogate, codepoint above U+10FFFF. Each byte that
  # cannot continue what came before starts a subpart of its own, so the
  # count follows the bytes rather than the run.
  assert_equal "\u{FFFD}" * 2, "\xC0\xAF".scrub             # overlong "/"
  assert_equal "\u{FFFD}" * 3, "\xED\xA0\x80".scrub         # surrogate U+D800
  assert_equal "\u{FFFD}" * 4, "\xF4\x90\x80\x80".scrub     # > U+10FFFF
end

assert('String#scrub replaces each maximal subpart') do
  skip unless "\u3042".length == 1
  # A prefix that could still have grown into a character is one subpart:
  # E3 81 needs a third byte, F0 9F 98 a fourth.
  assert_equal "\u{FFFD}", "\xE3\x81".scrub
  assert_equal "\u{FFFD}", "\xF0\x9F\x98".scrub
  # E0 admits A0-BF as its second byte, so 80 ends the subpart at E0
  assert_equal "\u{FFFD}" * 3, "\xE0\x80\xAF".scrub
  # the example from Unicode 3.9: E1 80 | E2 | F0 91 92 | F1 BF BF BF
  assert_equal "\u{FFFD}" * 3 + "\u{7FFFF}",
               "\xE1\x80\xE2\xF0\x91\x92\xF1\xBF\xBF\xBF".scrub
  # the block form is handed the same subparts
  got = []
  "\xE0\x80\xAF".scrub { |bad| got << bad.bytes; "?" }
  assert_equal [[0xE0], [0x80], [0xAF]], got
end

assert('String#scrub with replacement string') do
  skip unless "あ".length == 1
  assert_equal "abc?def", "abc\x80def".scrub("?")
  assert_equal "abcdef",  "abc\x80def".scrub("")
  assert_equal "abc<bad>def", "abc\x80def".scrub("<bad>")
end

assert('String#scrub raises on invalid replacement') do
  skip unless "あ".length == 1
  assert_raise(ArgumentError) { "abc\x80".scrub("\xFF") }
end

assert('String#scrub with block') do
  skip unless "あ".length == 1
  assert_equal "abc<80>def",
               "abc\x80def".scrub { |b| "<" + b.bytes.first.to_s(16) + ">" }
  # Block not called when string is already valid
  called = false
  "hello".scrub { |_| called = true; "X" }
  assert_false called
  # Multiple invalid runs each get their own block invocation
  result = "a\x80b\x81c".scrub { |b| "[#{b.bytes.first}]" }
  assert_equal "a[128]b[129]c", result
  # Non-String block return values are coerced via to_s (mruby leniency;
  # CRuby raises TypeError instead). Locking this in so the choice is
  # explicit and doesn't drift accidentally.
  assert_equal "abc42def", "abc\x80def".scrub { 42 }
end

assert('String#scrub no-op on non-UTF-8 build') do
  skip if "あ".length == 1
  # Method is still defined and returns a (string-equal) copy.
  assert_equal "abc\x80def", "abc\x80def".scrub
  assert_equal "abc\x80def", "abc\x80def".scrub("?")
  assert_equal "abc\x80def", "abc\x80def".scrub { |_| "?" }
end

assert('String#chars of a receiver longer than the GC arena') do
  # Each piece sits in the GC arena until it is pushed into the result, and
  # nothing took it back out, so a receiver of more characters than the arena
  # holds (MRB_GC_ARENA_SIZE, 100) overflowed it. The array came back
  # unprotected and the next method call on it read a collected object, which
  # only a build that collects on every allocation (MRB_GC_STRESS) reached.
  assert_equal 300, ("a" * 300).chars.size
  assert_equal "a", ("a" * 300).chars.last
  assert_equal 300, ("a" * 300).chars.join.size
end

assert('String#chars of a multibyte receiver longer than the GC arena') do
  assert_equal 300, ("\u3042" * 300).chars.size
  assert_equal "\u3042", ("\u3042" * 300).chars.last
end if UTF8STRING
