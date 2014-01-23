##
# String(utf8) Test

assert('String#[]') do
  assert_equal "ち", "こんにちわ世界"[3]
  assert_equal nil, "こんにちわ世界"[20]
  assert_equal "世", "こんにちわ世界"[-2]
  assert_equal "世界", "こんにちわ世界"[-2..-1]
  assert_equal "んに", "こんにちわ世界"[1,2]
  assert_equal "世", "こんにちわ世界"["世"]
end

assert('String#reverse', '15.2.10.5.29') do
  a = 'こんにちわ世界!'
  a.reverse
    
  assert_equal 'こんにちわ世界!', a
  assert_equal '!界世わちにんこ', 'こんにちわ世界!'.reverse
end

assert('String#reverse!', '15.2.10.5.30') do
  a = 'こんにちわ世界!'
  a.reverse!
    
  assert_equal '!界世わちにんこ', a
  assert_equal '!界世わちにんこ', 'こんにちわ世界!'.reverse!
end

assert('Invalid sequence') do
  assert_equal 5, "\xF8\x88\x80\x80\x80".size
  assert_equal 6, "\xFC\x84\x80\x80\x80\x80".size
end
