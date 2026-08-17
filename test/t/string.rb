##
# String ISO Test

UTF8STRING = __ENCODING__ == "UTF-8"
UNICODECASE = "\u00C4".downcase == "\u00E4"

assert('String', '15.2.10') do
  assert_equal Class, String.class
end

assert('String#<=>', '15.2.10.5.1') do
  a = '' <=> ''
  b = '' <=> 'not empty'
  c = 'not empty' <=> ''
  d = 'abc' <=> 'cba'
  e = 'cba' <=> 'abc'

  assert_equal  0, a
  assert_equal(-1, b)
  assert_equal  1, c
  assert_equal(-1, d)
  assert_equal  1, e
  assert_nil 'a' <=> 1024
end

assert('String#==', '15.2.10.5.2') do
  assert_equal 'abc', 'abc'
  assert_not_equal 'abc', 'cba'
end

# 'String#=~', '15.2.10.5.3' will be tested in mrbgems.

assert('String#+', '15.2.10.5.4') do
  assert_equal 'ab', 'a' + 'b'
end

assert('String#*', '15.2.10.5.5') do
  assert_equal 'aaaaa', 'a' * 5
  assert_equal '', 'a' * 0
  assert_raise(ArgumentError) { 'a' * -1 }
  assert_raise(TypeError) { 'a' * '1' }
  assert_raise(TypeError) { 'a' * nil }

  skip unless Object.const_defined?(:Float)
  assert_equal 'aa', 'a' * 2.1
  assert_raise(RangeError) { '' * 1e30 }
  assert_raise(RangeError) { '' * Float::INFINITY }
  assert_raise(RangeError) { '' * Float::NAN }
end

assert('String#[]', '15.2.10.5.6') do
  # length of args is 1
  assert_equal 'a', 'abc'[0]
  assert_equal 'c', 'abc'[-1]
  assert_nil 'abc'[10]
  assert_nil 'abc'[-10]
  assert_equal 'b', 'abc'[1.1] if Object.const_defined?(:Float)

  # length of args is 2
  assert_nil 'abc'[0, -1]
  assert_nil 'abc'[10, 0]
  assert_nil 'abc'[-10, 0]
  assert_equal '', 'abc'[0, 0]
  assert_equal 'bc', 'abc'[1, 2]

  # args is String
  assert_equal 'bc', 'abc'['bc']
  assert_nil 'abc'['XX']

  assert_raise(TypeError) { 'abc'[nil] }
end

assert('String#[](UTF-8)', '15.2.10.5.6') do
  assert_equal "ち", "こんにちは世界"[3]
  assert_equal nil, "こんにちは世界"[20]
  assert_equal "世", "こんにちは世界"[-2]
  assert_equal "世界", "こんにちは世界"[-2..-1]
  assert_equal "んに", "こんにちは世界"[1,2]
  assert_equal "世", "こんにちは世界"["世"]
end if UTF8STRING

assert('String#[](UTF-8) indexes a byte that spells no character on its own') do
  # Such a byte is a position of its own, which is what String#length counts
  # it as, and it takes that one position beside whole characters too.
  s = "\xED\xA0\x80"
  assert_equal "\xED", s[0]
  assert_equal "\xA0", s[1]
  assert_equal "\x80", s[2]
  assert_nil s[3]
  assert_equal "\xA0\x80", s[1, 2]
  assert_equal "b", "a\xE3\x81b"[3]
  assert_equal "\x80", "あ\x80"[1]
  assert_equal "あ", "\x80あ"[1]
end if UTF8STRING

assert('String#[](UTF-8) counts a negative index back from the end') do
  # Stepping back off the first character leaves the string, so a negative
  # index reaching past the head names no position rather than wrapping to
  # one. The length the range asks for is clamped to what is left after it.
  assert_equal "あ", "あい"[-2]
  assert_nil "あい"[-3]
  assert_equal "あ", "あい"[-2, 1]
  assert_nil "あい"[-3, 1]
  assert_equal "あい", "あい"[-2, 5]
end if UTF8STRING

assert('String#[] with Range') do
  a1 = 'abc'[1..0]
  b1 = 'abc'[1..1]
  c1 = 'abc'[1..2]
  d1 = 'abc'[1..3]
  e1 = 'abc'[1..4]
  f1 = 'abc'[0..-2]
  g1 = 'abc'[-2..3]
  h1 = 'abc'[3..4]
  i1 = 'abc'[4..5]
  j1 = 'abcdefghijklmnopqrstuvwxyz'[1..3]
  k1 = 'abcdefghijklmnopqrstuvwxyz'[-3..]
  a2 = 'abc'[1...0]
  b2 = 'abc'[1...1]
  c2 = 'abc'[1...2]
  d2 = 'abc'[1...3]
  e2 = 'abc'[1...4]
  f2 = 'abc'[0...-2]
  g2 = 'abc'[-2...3]
  h2 = 'abc'[3...4]
  i2 = 'abc'[4...5]
  j2 = 'abcdefghijklmnopqrstuvwxyz'[1...3]
  k2 = 'abcdefghijklmnopqrstuvwxyz'[-3...]

  assert_equal '', a1
  assert_equal 'b', b1
  assert_equal 'bc', c1
  assert_equal 'bc', d1
  assert_equal 'bc', e1
  assert_equal 'ab', f1
  assert_equal 'bc', g1
  assert_equal '', h1
  assert_nil i2
  assert_equal 'bcd', j1
  assert_equal 'xyz', k1
  assert_equal '', a2
  assert_equal '', b2
  assert_equal 'b', c2
  assert_equal 'bc', d2
  assert_equal 'bc', e2
  assert_equal 'a', f2
  assert_equal 'bc', g2
  assert_equal '', h2
  assert_nil i2
  assert_equal 'bc', j2
  assert_equal 'xyz', k2
end

assert('String#[] redefined on String itself reaches the redefinition') do
  # `OP_GETIDX` answers `s[1]` from C, and `OP_GETIDX0` answers `s[0]` the same
  # way, whenever the receiver's class is exactly `String`. Answering from C is
  # allowed only while `String#[]` is still the builtin those opcodes
  # reimplement, so both test the receiver against `mrb->idx_class[]`, which
  # the method table drops the moment `String#[]` is replaced. A redefinition
  # installed on `String` itself is therefore honored, as in CRuby, and not
  # only the subclass and singleton receivers that already failed the class
  # test for the other reason.
  String.class_eval do
    alias_method :__aref_before_test, :[]
    def [](*args)
      :overridden
    end
  end
  begin
    s = 'hello'
    sub = Class.new(String).new('hello')
    assert_equal :overridden, s[0]
    assert_equal :overridden, s[1]
    assert_equal :overridden, sub[0]
  ensure
    String.class_eval do
      alias_method :[], :__aref_before_test
      # `remove_method` comes from mruby-metaprog, which the core test build
      # does not have; the saved alias is harmless where it is missing.
      remove_method :__aref_before_test if respond_to?(:remove_method, true)
    end
  end
  # Aliasing the original implementation back makes `String#[]` resolve to it
  # again, which re-arms the opcodes.
  assert_equal 'h', 'hello'[0]
  assert_equal 'e', 'hello'[1]
end

assert('String#[]= redefined on String itself reaches the redefinition') do
  # `OP_SETIDX` answers `s[0] = 'X'` from C whenever the receiver's class is
  # exactly `String`, on the same terms as the `[]` test above: it tests the
  # receiver against `mrb->idx_class[]`, which the method table drops the
  # moment `String#[]=` is replaced. A redefinition that stores nothing makes
  # the difference visible in the receiver as well as in the return value.
  String.class_eval do
    alias_method :__aset_before_test, :[]=
    def []=(*args)
      $string_aset_redefinition_args = args
    end
  end
  begin
    s = 'hello'
    s[0] = 'X'
    seen = $string_aset_redefinition_args
    untouched = s.dup
    sub = Class.new(String).new('hello')
    sub[0] = 'X'
    seen_sub = $string_aset_redefinition_args
  ensure
    String.class_eval do
      alias_method :[]=, :__aset_before_test
      # `remove_method` comes from mruby-metaprog, which the core test build
      # does not have; the saved alias is harmless where it is missing.
      remove_method :__aset_before_test if respond_to?(:remove_method, true)
    end
    $string_aset_redefinition_args = nil
  end
  assert_equal [0, 'X'], seen
  assert_equal 'hello', untouched
  assert_equal [0, 'X'], seen_sub
  # Aliasing the original implementation back re-arms the opcode.
  s = 'hello'
  s[0] = 'X'
  assert_equal 'Xello', s
end

assert('String#[]= answers the same through the opcode and through a send') do
  # `s[x] = repl` is answered by `OP_SETIDX` in C, without a method lookup,
  # while `s.[]=(x, repl)` reaches `String#[]=` itself. The opcode
  # answers an Integer, String or Range index and a String replacement, and
  # leaves every other form to the method, so ask both ways and compare the
  # receiver each left behind.
  [0, 1, 4, -1, -5].each do |i|
    a = 'hello'; b = 'hello'
    a[i] = 'X'
    b.[]=(i, 'X')
    assert_equal b, a, "s[#{i}] = 'X'"
  end
  ['h', 'll', 'hello', ''].each do |sub|
    a = 'hello'; b = 'hello'
    a[sub] = 'X'
    b.[]=(sub, 'X')
    assert_equal b, a, "s[#{sub.inspect}] = 'X'"
  end
  [0..2, 1...3, -3..-1, 0..-1, 2..99, 3..1].each do |r|
    a = 'hello'; b = 'hello'
    a[r] = 'X'
    b.[]=(r, 'X')
    assert_equal b, a, "s[#{r.inspect}] = 'X'"
  end
  # Each loop above compares the two forms with each other, so anchor one case
  # of every index type to the result itself: a defect that made both forms
  # store nothing would agree with itself.
  a = 'hello'; a[1] = 'X'
  assert_equal 'hXllo', a
  a = 'hello'; a['ll'] = 'X'
  assert_equal 'heXo', a
  a = 'hello'; a[1..3] = 'X'
  assert_equal 'hXo', a
  # The forms the opcode leaves to the method raise what the method raises.
  assert_raise(IndexError) { 'hello'[99] = 'X' }
  assert_raise(IndexError) { 'hello'['zz'] = 'X' }
  assert_raise(IndexError) { 'hello'[99..100] = 'X' }
  assert_raise(TypeError) { 'hello'[0] = :sym }
  assert_raise(TypeError) { 'hello'[nil] = 'X' }
  assert_raise(FrozenError) { 'hello'.freeze[0] = 'X' }
end

assert('String#[]=') do
  # length of args is 1
  a = 'abc'
  assert_equal 'X', (a[0] = 'X')
  assert_equal 'Xbc', a

  b = 'abc'
  b[-1] = 'X'
  assert_equal 'abX', b

  c = 'abc'
  assert_raise(IndexError) do
    c[10] = 'X'
  end

  d = 'abc'
  assert_raise(IndexError) do
    d[-10] = 'X'
  end

  if Object.const_defined?(:Float)
    e = 'abc'
    e[1.1] = 'X'
    assert_equal 'aXc', e
  end

  f = 'abc'
  assert_equal 'X', f.[]=(0, 'X')
  assert_equal 'Xbc', f

  assert_raise(TypeError) { 'a'[0] = 1 }
  assert_raise(TypeError) { 'a'[:a] = '1' }

  # length of args is 2
  a1 = 'abc'
  assert_raise(IndexError) do
    a1[0, -1] = 'X'
  end

  b1 = 'abc'
  assert_raise(IndexError) do
    b1[10, 0] = 'X'
  end

  c1 = 'abc'
  assert_raise(IndexError) do
    c1[-10, 0] = 'X'
  end

  d1 = 'abc'
  d1[0, 0] = 'X'
  assert_equal 'Xabc', d1

  e1 = 'abc'
  assert_equal 'X', (e1[1, 3] = 'X')
  assert_equal 'aX', e1

  f1 = 'abc'
  assert_equal 'X', f1.[]=(0, 1, 'X')
  assert_equal 'Xbc', f1

  # args is RegExp
  # It will be tested in mrbgems.

  # args is String
  a3 = 'abc'
  assert_equal 'X', (a3['bc'] = 'X')
  assert_equal a3, 'aX'

  b3 = 'abc'
  assert_raise(IndexError) do
    b3['XX'] = 'Y'
  end

  c3 = 'abc'
  assert_equal 'X', c3.[]=('bc', 'X')
  assert_equal 'aX', c3

  assert_raise(TypeError) { 'a'[:a, 0] = '1' }
  assert_raise(TypeError) { 'a'[0, :a] = '1' }
  assert_raise(TypeError) { 'a'[0, 1] = 1 }
end

assert('String[]=(UTF-8)') do
  a = "➀➁➂➃➄"
  a[3] = "⚃"
  assert_equal "➀➁➂⚃➄", a

  b = "➀➁➂➃➄"
  b[3, 0] = "⛄"
  assert_equal "➀➁➂⛄➃➄", b

  c = "➀➁➂➃➄"
  c[3, 2] = "⚃⚄"
  assert_equal "➀➁➂⚃⚄", c

  d = "➀➁➂➃➄"
  d[5] = "⛄"
  assert_equal "➀➁➂➃➄⛄", d

  e = "➀➁➂➃➄"
  e[5, 0] = "⛄"
  assert_equal "➀➁➂➃➄⛄", e

  f = "➀➁➂➃➄"
  f[5, 2] = "⛄"
  assert_equal "➀➁➂➃➄⛄", f

  g = "➀➁➂➃➄"
  assert_raise(IndexError) { g[6] = "⛄" }

  h = "➀➁➂➃➄"
  assert_raise(IndexError) { h[6, 0] = "⛄" }

  i = "➀➁➂➃➄"
  assert_raise(IndexError) { i[6, 2] = "⛄" }

  j = "➀➁➂➃➄"
  j["➃"] = "⚃"
  assert_equal "➀➁➂⚃➄", j

  k = "➀➁➂➃➄"
  assert_raise(IndexError) { k["⛄"] = "⛇" }

  l = "➀➁➂➃➄"
  assert_nothing_raised { l["➂"] = "" }
  assert_equal "➀➁➃➄", l

  m = "➀➁➂➃➄"
  assert_raise(TypeError) { m["➂"] = nil }
  assert_equal "➀➁➂➃➄", m
end if UTF8STRING

assert('String#capitalize', '15.2.10.5.7') do
  a = 'abc'
  a.capitalize

  assert_equal 'abc', a
  assert_equal 'Abc', 'abc'.capitalize
end

assert('String#capitalize!', '15.2.10.5.8') do
  a = 'abc'
  a.capitalize!

  assert_equal 'Abc', a
  assert_equal nil, 'Abc'.capitalize!
end

assert('String#capitalize - Unicode') do
  # The first character takes title case, which is not always its upper case.
  assert_equal 'ǲ', 'ǳ'.capitalize
  assert_equal 'Ǳ', 'ǳ'.upcase
  # The rest takes lower case, whatever case it came in.
  assert_equal 'ǲabc', 'ǳabc'.capitalize
  assert_equal 'Aä', 'aÄ'.capitalize
  # A mapping that spells more than one character.
  assert_equal 'Fi', 'ﬁ'.capitalize
  # Georgian Mkhedruli upper cases to Mtavruli and title cases to itself, so
  # the two answers have to be told apart rather than shared.
  assert_equal 'Ა', 'ა'.upcase
  assert_equal 'ა', 'ა'.capitalize
  assert_nil 'ა'.capitalize!
end if UNICODECASE

assert('String#downcase - Unicode') do
  assert_equal 'äöü', 'ÄÖÜ'.downcase
  # Word final sigma is a mapping that reads its neighbours, which neither
  # this nor CRuby applies: both answer "σ" for the last one too.
  assert_equal 'σοφοσ', 'ΣΟΦΟΣ'.downcase
  # A mapping changes how many bytes a character takes: U+212A is three bytes
  # and lower cases to the one of "k".
  assert_equal 'k', "\u{212a}".downcase
  assert_equal 1, "\u{212a}".downcase.bytesize
  # And it can spell more characters than it was handed.
  assert_equal "i\u{307}", 'İ'.downcase
  assert_equal 2, 'İ'.downcase.length
  # A script without case has nothing to map.
  assert_equal '日本', '日本'.downcase
  assert_nil '日本'.downcase!
end if UNICODECASE

assert('String case conversion - bytes that spell no character') do
  # A run of bytes that spells no character has no case, and answering as
  # though it were the byte it starts with would hand back a string nobody
  # asked for, so the conversion refuses it.
  broken = "\xC3ABC"
  assert_raise(ArgumentError) { broken.downcase }
  assert_raise(ArgumentError) { broken.upcase }
  assert_raise(ArgumentError) { broken.capitalize }
  assert_raise_with_message(ArgumentError, 'input string invalid') { broken.downcase }
  # The receiver of a refused conversion stands as it was.
  assert_raise(ArgumentError) { broken.downcase! }
  assert_equal [195, 65, 66, 67], broken.bytes
  # ASCII ahead of the broken run does not save it, and neither does a
  # conversion that would have changed nothing.
  assert_raise(ArgumentError) { "abc\x80".downcase }
  assert_raise(ArgumentError) { "\x80".downcase }
end if UNICODECASE

assert('String#upcase - Unicode') do
  assert_equal 'ÄÖÜ', 'äöü'.upcase
  assert_equal 'ΣΟΦΟΣ', 'σοφος'.upcase
  # A mapping that spells more than one character.
  assert_equal 'SS', 'ß'.upcase
  assert_equal 2, 'ß'.upcase.length
  assert_equal 'FI', 'ﬁ'.upcase
  # And one that shortens: U+0131 is two bytes and upper cases to "I".
  assert_equal 'I', 'ı'.upcase
  assert_equal 1, 'ı'.upcase.bytesize
  assert_equal '日本', '日本'.upcase
  assert_nil '日本'.upcase!
end if UNICODECASE

assert('String#upcase - an answer that outgrows an embedded buffer') do
  # U+0390 is two bytes and upper cases to six, so a string short enough to
  # live inside its own object converts to one that cannot. The answer is
  # built beside the string, and a buffer that leaves an object carries over
  # what the object says it holds: the walk has to say how much it has
  # written, or the bytes written so far are dropped where the buffer moves.
  assert_equal "Ϊ́" * 4, ("ΐ" * 4).upcase
  assert_equal 24, ("ΐ" * 4).upcase.bytesize
  # The same crossing one character at a time, so wherever the boundary of an
  # embedded string falls, some length below walks over it.
  1.upto(12) do |n|
    assert_equal "Ϊ́" * n, ("ΐ" * n).upcase
  end
end if UNICODECASE

assert('String case conversion - ASCII only') do
  # The other reading of case: a build that converts by ASCII, whether by
  # MRB_USE_ASCII_CASE or by reading its strings as bytes, has no mapping above
  # ASCII, so a character that has one on the Unicode side stands as it was
  # while the ASCII beside it still converts.
  assert_equal 'Ä', 'Ä'.downcase
  assert_equal 'ä', 'ä'.upcase
  assert_equal 'Ä', 'Ä'.capitalize
  assert_equal 'äb', 'äB'.downcase
  assert_equal 'ÄB', 'Äb'.upcase
  assert_equal 'Äb', 'ÄB'.capitalize
  # A conversion that maps nothing is one that changed nothing.
  assert_nil 'Ä'.downcase!
  assert_nil 'ä'.upcase!
  assert_nil 'Ä'.capitalize!
  # Refusing a run of bytes that spells no character belongs to the walk over
  # characters; a walk that only knows ASCII hands the bytes back untouched.
  assert_equal [195, 97, 98, 99], "\xC3ABC".downcase.bytes
  assert_equal [195, 65, 66, 67], "\xC3ABC".upcase.bytes
end unless UNICODECASE

assert('String#chomp', '15.2.10.5.9') do
  a = 'abc'.chomp
  b = ''.chomp
  c = "abc\n".chomp
  d = "abc\n\n".chomp
  e = "abc\t".chomp("\t")
  f = "abc\n"

  f.chomp

  assert_equal 'abc', a
  assert_equal '', b
  assert_equal 'abc', c
  assert_equal "abc\n", d
  assert_equal 'abc', e
  assert_equal "abc\n", f
end

assert('String#chomp does not cut inside a character') do
  # The separator is matched byte by byte, so it can line up with the tail of
  # a character rather than a character of its own. Cutting there would leave
  # a string that is not UTF-8, so it counts as no match.
  assert_equal "あ", "あ".chomp("\x82")
  assert_equal "あ", "あ".chomp("\x81\x82")
  assert_equal "あい", "あい".chomp("\x84")
  assert_equal "aあ", "aあ".chomp("\x82")
  assert_nil "あ".chomp!("\x82")
  # a separator that is a whole character still cuts
  assert_equal "", "あ".chomp("あ")
  assert_equal "あ", "あい".chomp("い")
end if UTF8STRING

assert('String#chomp!', '15.2.10.5.10') do
  a = 'abc'
  b = ''
  c = "abc\n"
  d = "abc\n\n"
  e = "abc\t"

  a.chomp!
  b.chomp!
  c.chomp!
  d.chomp!
  e.chomp!("\t")

  assert_equal 'abc', a
  assert_equal '', b
  assert_equal 'abc', c
  assert_equal "abc\n", d
  assert_equal 'abc', e
end

assert('String#chop', '15.2.10.5.11') do
  a = ''.chop
  b = 'abc'.chop
  c = 'abc'

  c.chop

  assert_equal '', a
  assert_equal 'ab', b
  assert_equal 'abc', c
end

assert('String#chop(UTF-8)', '15.2.10.5.11') do
  a = ''.chop
  b = 'あいう'.chop
  c = "あ\nい".chop.chop

  assert_equal '', a
  assert_equal 'あい', b
  assert_equal 'あ', c
end if UTF8STRING

assert('String#chop!', '15.2.10.5.12') do
  a = ''
  b = 'abc'

  a.chop!
  b.chop!

  assert_equal a, ''
  assert_equal b, 'ab'
end

assert('String#chop!(UTF-8)', '15.2.10.5.12') do
  a = ''
  b = "あいうえ\n"
  c = "あいうえ\n"

  a.chop!
  b.chop!
  c.chop!
  c.chop!

  assert_equal a, ''
  assert_equal b, 'あいうえ'
  assert_equal c, 'あいう'
end if UTF8STRING

assert('String#chop! cuts where String#length counts a character') do
  # The last character starts where the character covering the last byte
  # starts, and a byte that no lead byte reaches is a character of its own.
  assert_equal "あ", "あ\x82".chop
  assert_equal "\x80\x80", "\x80\x80\x80".chop
  # A sequence RFC 3629 forbids spells no character, so its bytes stand alone.
  assert_equal "\xC0", "\xC0\x80".chop
  assert_equal "\xED\xA0", "\xED\xA0\x80".chop
  # A lead byte the string end cuts short reaches none of the bytes after it.
  assert_equal "a\xE3", "a\xE3\x81".chop
  # a whole character still goes at once, however many bytes it spells
  assert_equal "", "\u{1F600}".chop
  # and the \r\n pair is still taken together after one
  assert_equal "あ", "あ\r\n".chop
end if UTF8STRING

assert('String#downcase', '15.2.10.5.13') do
  a = 'ABC'.downcase
  b = 'ABC'

  b.downcase

  assert_equal 'abc', a
  assert_equal 'ABC', b
end

assert('String#downcase!', '15.2.10.5.14') do
  a = 'ABC'

  a.downcase!

  assert_equal 'abc', a
  assert_equal nil, 'abc'.downcase!
end

assert('String#each_line', '15.2.10.5.15') do
  a = "first line\nsecond line\nthird line"
  list = ["first line\n", "second line\n", "third line"]
  n_list = []

  a.each_line do |line|
    n_list << line
  end

  assert_equal list, n_list

  n_list.clear
  a.each_line("li") do |line|
    n_list << line
  end
  assert_equal ["first li", "ne\nsecond li", "ne\nthird li", "ne"], n_list
end

assert('String#empty?', '15.2.10.5.16') do
  a = ''
  b = 'not empty'

  assert_true a.empty?
  assert_false b.empty?
end

assert('String#eql?', '15.2.10.5.17') do
  assert_true 'abc'.eql?('abc')
  assert_false 'abc'.eql?('cba')
end

assert('String#gsub', '15.2.10.5.18') do
  assert_equal('aBcaBc', 'abcabc'.gsub('b', 'B'), 'gsub without block')
  assert_equal('aBcaBc', 'abcabc'.gsub('b'){|w| w.capitalize }, 'gsub with block')
  assert_equal('$a$a$',  '#a#a#'.gsub('#', '$'), 'mruby/mruby#847')
  assert_equal('$a$a$',  '#a#a#'.gsub('#'){|_w| '$' }, 'mruby/mruby#847 with block')
  assert_equal('$$a$$',  '##a##'.gsub('##', '$$'), 'mruby/mruby#847 another case')
  assert_equal('$$a$$',  '##a##'.gsub('##'){|_w| '$$' }, 'mruby/mruby#847 another case with block')
  assert_equal('A',      'a'.gsub('a', 'A'))
  assert_equal('A',      'a'.gsub('a'){|w| w.capitalize })
  assert_equal("<a><><>", 'a'.gsub('a', '<\0><\1><\2>'))
  assert_equal(".h.e.l.l.o.", "hello".gsub("", "."))
  a = []
  assert_equal(".h.e.l.l.o.", "hello".gsub("") { |i| a << i; "." })
  assert_equal(["", "", "", "", "", ""], a)
  assert_raise(ArgumentError) { "".gsub }
  assert_raise(ArgumentError) { "".gsub("", "", "") }
end

assert('String#gsub with backslash') do
  s = 'abXcdXef'
  assert_equal 'ab<\\>cd<\\>ef',    s.gsub('X', '<\\\\>')
  assert_equal 'ab<X>cd<X>ef',      s.gsub('X', '<\\&>')
  assert_equal 'ab<X>cd<X>ef',      s.gsub('X', '<\\0>')
  assert_equal 'ab<ab>cd<abXcd>ef', s.gsub('X', '<\\`>')
  assert_equal 'ab<cdXef>cd<ef>ef', s.gsub('X', '<\\\'>')
end

assert('String#gsub!', '15.2.10.5.19') do
  a = 'abcabc'
  a.gsub!('b', 'B')

  b = 'abcabc'
  b.gsub!('b') { |w| w.capitalize }

  assert_equal 'aBcaBc', a
  assert_equal 'aBcaBc', b
end

assert('String#hash', '15.2.10.5.20') do
  a = 'abc'

  assert_equal 'abc'.hash, a.hash
end

assert('String#include?', '15.2.10.5.21') do
  assert_true 'abc'.include?('a')
  assert_false 'abc'.include?('d')
end

assert('String#index', '15.2.10.5.22') do
  assert_equal 0, 'abc'.index('a')
  assert_nil 'abc'.index('d')
  assert_equal 3, 'abcabc'.index('a', 1)
  assert_equal 5, "hello".index("", 5)
  assert_equal nil, "hello".index("", 6)
  assert_equal 3, "hello".index("l", -2)
  assert_raise(ArgumentError) { "hello".index }
  assert_raise(TypeError) { "hello".index(101) }
end

assert('String#index(UTF-8)', '15.2.10.5.22') do
  assert_equal 0, '⓿➊➋➌➍➎'.index('⓿')
  assert_nil '⓿➊➋➌➍➎'.index('➓')
  assert_equal 6, '⓿➊➋➌➍➎⓿➊➋➌➍➎'.index('⓿', 1)
  assert_equal 6, '⓿➊➋➌➍➎⓿➊➋➌➍➎'.index('⓿', -7)
  assert_equal 6, "⓿➊➋➌➍➎".index("", 6)
  assert_equal nil, "⓿➊➋➌➍➎".index("", 7)
  # A needle whose bytes spell no character is found nowhere; the
  # byte-indexed counterparts live in mruby-string-ext's tests, where
  # String#b is available to write them.
  assert_nil '⓿➊➋➌➍➎'.index("\xe2")
  assert_nil '⓿➊➋➌➍➎'.index("\xe3")
  assert_equal 6, "\xd1\xd1\xd1\xd1\xd1\xd1⓿➊➋➌➍➎".index('⓿')
end if UTF8STRING

assert('String#initialize', '15.2.10.5.23') do
  a = ''
  a.__send__(:initialize,'abc')
  assert_equal 'abc', a

  a.__send__(:initialize,'abcdefghijklmnopqrstuvwxyz')
  assert_equal 'abcdefghijklmnopqrstuvwxyz', a
end

assert('String#initialize_copy', '15.2.10.5.24') do
  a = ''
  a.__send__(:initialize_copy, 'abc')

  assert_equal 'abc', a
end

assert('String#intern', '15.2.10.5.25') do
  assert_equal :abc, 'abc'.intern
end

assert('String#length', '15.2.10.5.26') do
  assert_equal 3, 'abc'.length
end

assert('String#length(UTF-8)', '15.2.10.5.26') do
  assert_equal 3, 'あいう'.length

  # A substring too long to embed shares the parent's buffer, so the byte
  # after its last one belongs to the parent instead of terminating it.
  s = ('あ' * 40)[0, 20]
  assert_equal 60, s.bytesize
  assert_equal 20, s.length
  assert_nil s[20]
  assert_equal 20, ('あ' * 40)[10, 20].length
  assert_equal 10, ("\u{1F600}" * 20)[0, 10].length

  # These answered correctly all along: a substring short enough to embed is
  # copied and terminated, one reaching the parent's end stops at the parent's
  # terminator, and a run of non-ASCII bytes ends on an ASCII one.
  assert_equal 2, ('あ' * 40)[0, 2].length
  assert_equal 20, ('あ' * 40)[20, 20].length
  assert_equal 20, ('aあ' * 30)[0, 20].length

  # Cut in the middle of a character, the loose bytes count one each,
  # whether the string ends in the parent's buffer or in its own.
  assert_equal 21, ('あ' * 40).byteslice(0, 59).length
  assert_equal 2, "\xe3\x81".length
end if UTF8STRING

# 'String#match', '15.2.10.5.27' will be tested in mrbgems.

assert('String#replace', '15.2.10.5.28') do
  a = ''
  a.replace('abc')

  assert_equal 'abc', a
  assert_equal 'abc', 'cba'.replace(a)

  b = 'abc' * 10
  c = ('cba' * 10).dup
  b.replace(c)
  c.replace(b)
  assert_equal c, b

  # shared string
  s = "foo" * 100
  a = s[10, 90]                # create shared string
  assert_equal("", s.replace(""))    # clear
  assert_equal("", s)          # s is cleared
  assert_not_equal("", a)      # a should not be affected
end

assert('String#reverse', '15.2.10.5.29') do
  a = 'abc'
  a.reverse

  assert_equal 'abc', a
  assert_equal 'cba', 'abc'.reverse
end

assert('String#reverse(UTF-8)', '15.2.10.5.29') do
  a = 'こんにちは世界!'
  a.reverse

  assert_equal 'こんにちは世界!', a
  assert_equal '!界世はちにんこ', 'こんにちは世界!'.reverse
  assert_equal 'あ', 'あ'.reverse
end if UTF8STRING

assert('String#reverse!', '15.2.10.5.30') do
  a = 'abc'
  a.reverse!

  assert_equal 'cba', a
  assert_equal 'cba', 'abc'.reverse!
end

assert('String#reverse!(UTF-8)', '15.2.10.5.30') do
  a = 'こんにちは世界!'
  a.reverse!

  assert_equal '!界世はちにんこ', a
  assert_equal '!界世はちにんこ', 'こんにちは世界!'.reverse!

  b = 'あ'
  b.reverse!
  assert_equal 'あ', b
end if UTF8STRING

assert('String#rindex', '15.2.10.5.31') do
  assert_equal 0, 'abc'.rindex('a')
  assert_equal 0, 'abc'.rindex('a', 3)
  assert_nil 'abc'.rindex('a', -4)
  assert_nil 'abc'.rindex('d')
  assert_equal 6, 'abcabc'.rindex('')
  assert_equal 3, 'abcabc'.rindex('a')
  assert_equal 0, 'abcabc'.rindex('a', 1)
  assert_equal 3, 'abcabc'.rindex('a', 4)
  assert_equal 0, 'abcabc'.rindex('a', -4)
  assert_raise(ArgumentError) { "hello".rindex }
  assert_raise(TypeError) { "hello".rindex(101) }
end

assert('String#rindex(UTF-8)', '15.2.10.5.31') do
  str = "こんにちは世界!\nこんにちは世界!"
  assert_nil str.rindex('さ')
  assert_equal 12, str.rindex('ち')
  assert_equal 3, str.rindex('ち', 10)
  assert_equal 3, str.rindex('ち', -6)

  broken = "\xf0☀\xf1☁\xf2☂\xf3☃\xf0☀\xf1☁\xf2☂\xf3☃"
  assert_nil broken.rindex("\x81") # "\x81" is a part of "☁" ("\xe2\x98\x81")
  assert_equal 11, broken.rindex("☁")
  assert_equal 11, broken.rindex("☁", 12)
  assert_equal 11, broken.rindex("☁", 11)
  assert_equal  3, broken.rindex("☁", 10)
end if UTF8STRING

assert('String#rindex reaches the first character from a negative position') do
  # A negative `pos` counts characters back from the end, so minus the length
  # names the first character rather than a step past it.
  assert_equal 0, "あいう".rindex("あ", -3)
  assert_nil "あいう".rindex("あ", -4)
  assert_equal 3, "あいうあ".rindex("あ", -1)
  assert_equal 0, "あいうあ".rindex("あ", -4)
  assert_nil "あいうあ".rindex("あ", -5)
end if UTF8STRING

assert('String#rindex steps by the characters String#length counts') do
  # A byte that no lead byte reaches spells no character with its neighbours,
  # so it is one position of its own, which is what #length counts it as.
  str = "あ\x80x"
  assert_equal 3, str.length
  assert_equal 0, str.rindex("あ", -2)
  # The byte is a position of its own, but a needle spelling no character is
  # found nowhere, so neither direction reports it. They agree, which is what
  # this block is about.
  assert_nil str.rindex("\x80")
  assert_equal str.index("\x80"), str.rindex("\x80")

  # Searching backward from `pos` may not answer a position after it.
  assert_nil str.rindex("x", 1)
  assert_equal 2, str.rindex("x", 2)

  # A sequence RFC 3629 forbids spells no character either, and its bytes
  # stand alone the same way.
  assert_equal 3, "\xC0\x80a".length
  assert_nil "\xC0\x80a".rindex("\xC0")
  assert_nil "\xC0\x80a".rindex("\x80")
  assert_equal 3, "\xED\xA0\x80".length
  assert_nil "\xED\xA0\x80".rindex("\x80")

  # A lead byte the string end cuts short reaches none of the bytes that
  # follow it, so those stand alone too.
  assert_equal 3, "a\xE3\x81".length
  assert_nil "a\xE3\x81".rindex("\xE3")
  assert_nil "a\xE3\x81".rindex("\x81")
end if UTF8STRING

assert('a byte search refuses an offset inside a character') do
  # An offset that lands inside a character names no position the string has,
  # so searching from it is refused rather than started from the middle of
  # one. The boundaries are the ones #length counts over.
  s = "aあb"          # 61 E3 81 82 62; boundaries 0, 1, 4, 5
  assert_equal 4, s.byteindex("b", 1)
  assert_equal 4, s.byteindex("b", 4)
  assert_raise(IndexError) { s.byteindex("b", 2) }
  assert_raise(IndexError) { s.byteindex("b", 3) }
  assert_raise(IndexError) { s.byterindex("a", 2) }
  # a negative offset is counted from the end first, then asked the same
  assert_equal 4, s.byteindex("b", -1)
  assert_raise(IndexError) { s.byteindex("b", -2) }
  # past either end is out of range rather than off a boundary
  assert_nil s.byteindex("b", 6)
  assert_nil s.byteindex("b", -6)
  assert_equal 0, s.byterindex("a", 99)
  # ASCII has a boundary at every byte, and so does a byte-indexed string
  assert_equal 2, "abc".byteindex("c", 1)
end if UTF8STRING

assert('String#byterindex searches bytes') do
  # `byterindex` answers byte positions, so it walks bytes the way
  # `byteindex` does. Walking characters instead, it passed over every byte
  # inside a multi-byte sequence and reported nothing there.
  str = "aあb" # "\x61\xe3\x81\x82\x62"
  assert_equal 4, str.byterindex("b")
  # Whatever a needle that spells no character answers, the two directions
  # answer it alike, which is what this block is about. On a UTF-8 build that
  # is nil, since such a needle names nothing to look for; the byte search
  # itself is asked of a byte-indexed subject in mruby-string-ext's tests,
  # where String#b is available to write one.
  assert_equal str.byteindex("\x81"), str.byterindex("\x81")
  if UTF8STRING
    assert_nil str.byterindex("\xe3")
    assert_nil str.byterindex("\x81")
    assert_nil str.byterindex("\x81", 1)
  end

  assert_equal 3, 'abcabc'.byterindex('a')
  assert_equal 0, 'abcabc'.byterindex('a', 1)
  assert_equal 6, 'abcabc'.byterindex('')
  assert_nil 'abc'.byterindex('d')
end

# assert('String#scan', '15.2.10.5.32') do
#   # Not implemented yet
# end

assert('String#size', '15.2.10.5.33') do
  assert_equal 3, 'abc'.size
end

assert('String#size(UTF-8)', '15.2.10.5.33') do
  str = 'こんにちは世界!'
  assert_equal 8, str.size
  assert_not_equal str.bytesize, str.size
  assert_equal 2, str[1, 2].size
end if UTF8STRING

assert('String#size(UTF-8) counts invalid sequences per byte') do
  # RFC 3629: overlong forms, UTF-16 surrogates, and code points above
  # U+10FFFF are not characters, so each of their bytes counts on its own
  assert_equal 2, "\xC0\x80".size          # overlong NUL
  assert_equal 3, "\xE0\x9F\xBF".size      # overlong (< U+0800)
  assert_equal 3, "\xED\xA0\x80".size      # surrogate U+D800
  assert_equal 4, "\xF0\x8F\xBF\xBF".size  # overlong (< U+10000)
  assert_equal 4, "\xF4\x90\x80\x80".size  # above U+10FFFF
  assert_equal 4, "\xF5\x80\x80\x80".size  # above U+10FFFF
  assert_equal 1, "\u{D7FF}".size          # last code point before surrogates
  assert_equal 1, "\u{E000}".size          # first code point after surrogates
  assert_equal 1, "\u{10FFFF}".size        # largest valid code point
end if UTF8STRING

assert('String#size(UTF-8) counts a broken sequence beside whole characters') do
  # The bytes of a broken sequence count one each wherever they sit, so a
  # string that carries one counts more positions than it holds characters.
  assert_equal 4, "abc\x80".size
  assert_equal 4, "\x80abc".size
  assert_equal 2, "あ\x80".size
  assert_equal 2, "\x80あ".size
  assert_equal 4, "a\xE3\x81b".size
  assert_equal 4, "\u{1F600}\xC0\xAF\xC0".size
end if UTF8STRING

assert('String#slice', '15.2.10.5.34') do
  # length of args is 1
  a = 'abc'.slice(0)
  b = 'abc'.slice(-1)
  c = 'abc'.slice(10)
  d = 'abc'.slice(-10)

  # length of args is 2
  a1 = 'abc'.slice(0, -1)
  b1 = 'abc'.slice(10, 0)
  c1 = 'abc'.slice(-10, 0)
  d1 = 'abc'.slice(0, 0)
  e1 = 'abc'.slice(1, 2)

  # slice of shared string
  e11 = e1.slice(0)

  # args is RegExp
  # It will be tested in mrbgems.

  # args is String
  a3 = 'abc'.slice('bc')
  b3 = 'abc'.slice('XX')

  assert_equal 'a', a
  assert_equal 'c', b
  assert_nil c
  assert_nil d
  assert_nil a1
  assert_nil b1
  assert_nil c1
  assert_equal '', d1
  assert_equal 'bc', e1
  assert_equal 'b', e11
  assert_equal 'bc', a3
  assert_nil b3
end

# TODO Broken ATM
assert('String#split', '15.2.10.5.35') do
  # without RegExp behavior is actually unspecified
  assert_equal ['abc', 'abc', 'abc'], 'abc abc abc'.split
  assert_equal ["a", "b", "c", "", "d"], 'a,b,c,,d'.split(',')
  assert_equal ['abc', 'abc', 'abc'], 'abc abc abc'.split(nil)
  assert_equal ['a', 'b', 'c'], 'abc'.split("")
end

assert('String#split(UTF-8)', '15.2.10.5.35') do
  got = "こんにちは世界!".split('')
  assert_equal ['こ', 'ん', 'に', 'ち', 'は', '世', '界', '!'], got
  got = "こんにちは世界!".split('に')
  assert_equal ['こん', 'ちは世界!'], got
end if UTF8STRING

assert('String#sub', '15.2.10.5.36') do
  assert_equal 'aBcabc', 'abcabc'.sub('b', 'B')
  assert_equal 'aBcabc', 'abcabc'.sub('b') { |w| w.capitalize }
  assert_equal 'aa$', 'aa#'.sub('#', '$')
  assert_equal '.abc', "abc".sub("", ".")

  str = "abc"
  miss = str.sub("X", "Z")
  assert_equal str, miss
  assert_not_same str, miss

  a = []
  assert_equal '.abc', "abc".sub("") { |i| a << i; "." }
  assert_equal [""], a
end

assert('String#sub with backslash') do
  s = 'abXcdXef'
  assert_equal 'ab<\\>cdXef',    s.sub('X', '<\\\\>')
  assert_equal 'ab<X>cdXef',     s.sub('X', '<\\&>')
  assert_equal 'ab<X>cdXef',     s.sub('X', '<\\0>')
  assert_equal 'ab<ab>cdXef',    s.sub('X', '<\\`>')
  assert_equal 'ab<cdXef>cdXef', s.sub('X', '<\\\'>')
end

assert('String#sub!', '15.2.10.5.37') do
  a = 'abcabc'
  a.sub!('b', 'B')

  b = 'abcabc'
  b.sub!('b') { |w| w.capitalize }

  assert_equal 'aBcabc', a
  assert_equal 'aBcabc', b
end

assert('String#to_f', '15.2.10.5.38') do
  assert_operator(0.0, :eql?, ''.to_f)
  assert_operator(123456789.0, :eql?, '123456789'.to_f)
  assert_operator(12345.6789, :eql?, '12345.6789'.to_f)
  assert_operator(0.0, :eql?, '1e-2147483648'.to_f)
  assert_operator(Float::INFINITY, :eql?, '1e2147483648'.to_f)
  assert_operator(0.0, :eql?, 'a'.to_f)
  assert_operator(4.0, :eql?, '4a5'.to_f)
  assert_operator(12.0, :eql?, '1_2__3'.to_f)
  assert_operator(123.0, :eql?, '1_2_3'.to_f)
  assert_operator(68.0, :eql?, '68_'.to_f)
  assert_operator(68.0, :eql?, '68._7'.to_f)
  assert_operator(68.7, :eql?, '68.7_'.to_f)
  assert_operator(68.7, :eql?, '68.7_ '.to_f)
  assert_operator(6.0, :eql?, '6 8.7'.to_f)
  assert_operator(68.0, :eql?, '68. 7'.to_f)
  assert_operator(0.0, :eql?, '_68'.to_f)
  assert_operator(0.0, :eql?, ' _68'.to_f)
  assert_operator(12.34, :eql?, '1_2.3_4'.to_f)
  assert_operator(12.3, :eql?, '1_2.3__4'.to_f)
  assert_operator(0.9, :eql?, '.9'.to_f)
  assert_operator(0.9, :eql?, "\t\r\n\f\v .9 \t\r\n\f\v".to_f)
  # an extremely long digit string must not overflow the truncated-digit
  # counter; the value is far beyond Float range, so it saturates to
  # Infinity instead of triggering signed-overflow UB (#6958)
  assert_operator(Float::INFINITY, :eql?, ('9' * 200000).to_f)
end if Object.const_defined?(:Float)

assert('String#to_i', '15.2.10.5.39') do
  assert_operator 0, :eql?, ''.to_i
  assert_operator 32143, :eql?, '32143'.to_i
  assert_operator 10, :eql?, 'a'.to_i(16)
  assert_operator 4, :eql?, '100'.to_i(2)
  assert_operator 1_000, :eql?, '1_000'.to_i
  assert_operator 0, :eql?, 'a'.to_i
  assert_operator 4, :eql?, '4a5'.to_i
  assert_operator 12, :eql?, '1_2__3'.to_i
  assert_operator 123, :eql?, '1_2_3'.to_i
  assert_operator 68, :eql?, '68_'.to_i
  assert_operator 68, :eql?, '68_ '.to_i
  assert_operator 0, :eql?, '_68'.to_i
  assert_operator 0, :eql?, ' _68'.to_i
  assert_operator 68, :eql?, "\t\r\n\f\v 68 \t\r\n\f\v".to_i
  assert_operator 6, :eql?, ' 6 8 '.to_i
end

assert('String#to_s', '15.2.10.5.40') do
  assert_equal 'abc', 'abc'.to_s
end

assert('String#to_sym', '15.2.10.5.41') do
  assert_equal :abc, 'abc'.to_sym
end

assert('String#upcase', '15.2.10.5.42') do
  a = 'abc'.upcase
  b = 'abc'

  b.upcase

  assert_equal 'ABC', a
  assert_equal 'abc', b
end

assert('String#upcase!', '15.2.10.5.43') do
  a = 'abc'

  a.upcase!

  assert_equal 'ABC', a
  assert_equal nil, 'ABC'.upcase!

  a = 'abcdefghijklmnopqrstuvwxyz'
  b = a.dup
  a.upcase!
  b.upcase!
  assert_equal 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', b
end

assert('String#inspect', '15.2.10.5.46') do
  assert_equal "\"\\x00\"", "\0".inspect
  assert_equal "\"foo\"", "foo".inspect
  # a byte spelled out as hex reads out in upper case, the way CRuby writes it
  assert_equal "\"\\xAB\"", "\xAB".inspect
  if UTF8STRING
    assert_equal '"る"', "る".inspect
  else
    assert_equal '"\xE3\x82\x8B"', "る".inspect
  end

  # should not raise an exception - regress #1210
  assert_nothing_raised do
    ("\1" * 100).inspect
  end
end

# Not ISO specified

assert('String interpolation (mrb_str_concat for shared strings)') do
  a = "A" * 32
  assert_equal "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA:", "#{a}:"
end

assert('String#bytes') do
  str1 = "hello"
  bytes1 = [104, 101, 108, 108, 111]

  str2 = "\xFF"
  bytes2 = [0xFF]

  assert_equal bytes1, str1.bytes
  assert_equal bytes2, str2.bytes
end

assert('String#each_byte') do
  str1 = "hello"
  bytes1 = [104, 101, 108, 108, 111]
  bytes2 = []

  str1.each_byte {|b| bytes2 << b }

  assert_equal bytes1, bytes2
end

assert('String#freeze') do
  str = "hello"
  str.freeze

  assert_raise(FrozenError) { str.upcase! }
end

assert('String literal concatenation') do
  assert_equal 2, ("A" "B").size
  assert_equal 3, ('A' "B" 'C').size
  assert_equal 4, (%(A) "B#{?C}" "D").size
end

assert('String#getbyte') do
  str1 = "hello"
  bytes1 = [104, 101, 108, 108, 111]
  assert_equal bytes1[0], str1.getbyte(0)
  assert_equal bytes1[-1], str1.getbyte(-1)
  assert_equal bytes1[6], str1.getbyte(6)

  str2 = "\xFF"
  bytes2 = [0xFF]
  assert_equal bytes2[0], str2.getbyte(0)
end

assert('String#setbyte') do
  str1 = "hello"
  h = "H".getbyte(0)
  str1.setbyte(0, h)
  assert_equal(h, str1.getbyte(0))
  assert_equal("Hello", str1)
end

assert('String#byteslice') do
  str1 = "hello"
  str2 = "\u3042ab"  # "\xE3\x81\x82ab"

  assert_equal("h", str1.byteslice(0))
  assert_equal("e", str1.byteslice(1))
  assert_equal(nil, str1.byteslice(5))
  assert_equal("o", str1.byteslice(-1))
  assert_equal(nil, str1.byteslice(-6))
  assert_equal("\xE3", str2.byteslice(0))
  assert_equal("\x81", str2.byteslice(1))
  assert_equal(nil, str2.byteslice(5))
  assert_equal("b", str2.byteslice(-1))
  assert_equal(nil, str2.byteslice(-6))

  assert_equal("", str1.byteslice(0, 0))
  assert_equal(str1, str1.byteslice(0, 6))
  assert_equal("el", str1.byteslice(1, 2))
  assert_equal("", str1.byteslice(5, 1))
  assert_equal("o", str1.byteslice(-1, 6))
  assert_equal(nil, str1.byteslice(-6, 1))
  assert_equal(nil, str1.byteslice(0, -1))
  assert_equal("", str2.byteslice(0, 0))
  assert_equal(str2, str2.byteslice(0, 6))
  assert_equal("\x81\x82", str2.byteslice(1, 2))
  assert_equal("", str2.byteslice(5, 1))
  assert_equal("b", str2.byteslice(-1, 6))
  assert_equal(nil, str2.byteslice(-6, 1))
  assert_equal(nil, str2.byteslice(0, -1))

  assert_equal("ell", str1.byteslice(1..3))
  assert_equal("el", str1.byteslice(1...3))
  assert_equal("h", str1.byteslice(0..0))
  assert_equal("", str1.byteslice(5..0))
  assert_equal("o", str1.byteslice(4..5))
  assert_equal(nil, str1.byteslice(6..0))
  assert_equal("", str1.byteslice(-1..0))
  assert_equal("llo", str1.byteslice(-3..5))
  assert_equal("\x81\x82a", str2.byteslice(1..3))
  assert_equal("\x81\x82", str2.byteslice(1...3))
  assert_equal("\xE3", str2.byteslice(0..0))
  assert_equal("", str2.byteslice(5..0))
  assert_equal("b", str2.byteslice(4..5))
  assert_equal(nil, str2.byteslice(6..0))
  assert_equal("", str2.byteslice(-1..0))
  assert_equal("\x82ab", str2.byteslice(-3..5))

  assert_raise(ArgumentError) { str1.byteslice }
  assert_raise(ArgumentError) { str1.byteslice(1, 2, 3) }
  assert_raise(TypeError) { str1.byteslice("1") }
  assert_raise(TypeError) { str1.byteslice("1", 2) }
  assert_raise(TypeError) { str1.byteslice(1, "2") }
  assert_raise(TypeError) { str1.byteslice(1..2, 3) }

  skip unless Object.const_defined?(:Float)
  assert_equal("o", str1.byteslice(4.0))
  assert_equal("\x82ab", str2.byteslice(2.0, 3.0))
end

assert('String#bytesplice') do
  # range, replace (len1=len2)
  a = "0123456789"
  assert_equal "0ab3456789", a.bytesplice(1..2, "ab")

  # range, replace (len1>len2)
  a = "0123456789"
  assert_equal "0ab456789", a.bytesplice(1..3, "ab")

  # range, replace (len1<len2)
  a = "0123456789"
  assert_equal "0ab23456789", a.bytesplice(1..1, "ab")

  # idx, len, replace (len1=len2)
  a = "0123456789"
  assert_equal "0ab3456789", a.bytesplice(1, 2, "ab")

  # idx, len, replace (len1>len2)
  a = "0123456789"
  assert_equal "0ab456789", a.bytesplice(1, 3, "ab")

  # idx, len, replace (len1<len2)
  a = "0123456789"
  assert_equal "0ab23456789", a.bytesplice(1, 1, "ab")

  b = "abcdefg"
  # range, replace, range (len1=len2)
  a = "0123456789"
  assert_equal "0ab3456789", a.bytesplice(1..2, b, 0..1)

  # range, replace, range (len1>len2)
  a = "0123456789"
  assert_equal "0bc456789", a.bytesplice(1..3, b, 1..2)

  # range, replace, range (len1<len2)
  a = "0123456789"
  assert_equal "0cd23456789", a.bytesplice(1..1, b, 2..3)

  # idx, len, replace, idx, len (len1=len2)
  a = "0123456789"
  assert_equal "0ab3456789", a.bytesplice(1, 2, b, 0, 2)

  # idx, len, replace, idx, len (len1>len2)
  a = "0123456789"
  assert_equal "0bc456789", a.bytesplice(1, 3, b, 1, 2)

  # idx, len, replace, idx, len (len1<len2)
  a = "0123456789"
  assert_equal "0cd23456789", a.bytesplice(1, 1, b, 2, 2)

  # check the object type to replace
  assert_raise(TypeError) { "0123456789".bytesplice(1, 1, Object.new) }

  # check the overflow to index and length (to be pass without crash)
  assert_nothing_raised { "0123456789".bytesplice(8, ~(-1 << 31), "ab") } # for MRB_INT32
  # The shift width comes from a variable because `1 << 63` written out is
  # constant folded, and the fold fails while this file is compiled on
  # MRB_INT32 without bigint, dropping every test in it.
  shift = 63
  assert_nothing_raised { begin; "0123456789".bytesplice(8, ~(-1 << shift), "ab"); rescue ArgumentError, RangeError; end } # for MRB_INT64

  # check the negative index
  assert_equal "0ab3456789", "0123456789".bytesplice(-9, 2, "ab")
  assert_equal "ab23456789", "0123456789".bytesplice(-10, 2, "ab")
  assert_raise(IndexError) { "0123456789".bytesplice(-11, 2, "ab") }

  # check the negative length
  assert_raise(IndexError) { "0123456789".bytesplice(3, -4, "ab") }

  # with an empty string
  assert_equal "012789", "0123456789".bytesplice(3, 4, "")
end

assert('String#bytesplice on a shared buffer') do
  # A longer replacement grows the string and then writes from `idx1`, which
  # a string sharing the same buffer still reads, so growing has to take the
  # buffer away from it. Each string is shortened before it is shared, to
  # leave spare capacity behind: one that has none cannot be grown in place
  # anyway, so it would not tell the two behaviours apart.
  a = "a" * 100 + "z" * 100
  a.bytesplice(100, 100, "")
  a_slice = a[0, 60]
  a.bytesplice(0, 1, "1234567890")
  assert_equal "1234567890" + "a" * 99, a
  assert_equal "a" * 60, a_slice

  # The sharer is the one that grows, ending below what its parent holds.
  # Contract rather than detection: that is already the case where a growth
  # keeping the buffer would have to take a copy regardless.
  b = "b" * 100 + "y" * 100
  b.bytesplice(100, 100, "")
  b_slice = b[0, 60]
  b_slice.bytesplice(0, 1, "1234567890")
  assert_equal "1234567890" + "b" * 59, b_slice
  assert_equal "b" * 100, b
end
