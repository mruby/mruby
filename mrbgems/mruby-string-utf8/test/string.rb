# -*- coding: utf-8 -*-
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

assert('String#size') do
  str = 'こんにちわ世界!'
  assert_equal 8, str.size
  assert_not_equal str.bytesize, str.size
  assert_equal 2, str[1, 2].size
end

assert('String#index') do
  str = "こんにちわ世界!\nこんにちわ世界!"
  assert_nil str.index('さ')
  assert_equal 3, str.index('ち')
  assert_equal 12, str.index('ち', 10)
  assert_equal nil, str.index("さ")
end

assert('String#ord') do
  got = "こんにちわ世界!".split('').map {|x| x.ord}
  expect = [0x3053,0x3093,0x306b,0x3061,0x308f,0x4e16,0x754c,0x21]
  assert_equal expect, got
end

assert('String#split') do
  got = "こんにちわ世界!".split('')
  assert_equal ['こ', 'ん', 'に', 'ち', 'わ', '世', '界', '!'], got
  got = "こんにちわ世界!".split('に')
  assert_equal ['こん', 'ちわ世界!'], got
end

assert('String#rindex') do
  str = "こんにちわ世界!\nこんにちわ世界!"
  assert_nil str.index('さ')
  assert_equal 12, str.rindex('ち')
  assert_equal 3, str.rindex('ち', 10)
end
