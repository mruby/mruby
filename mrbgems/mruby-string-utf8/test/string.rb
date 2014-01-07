##
# String(utf8) Test

assert('String#[]') do
  assert_equal "ち", "こんにちわ世界"[3]
  assert_equal nil, "こんにちわ世界"[20]
  assert_equal "世", "こんにちわ世界"[-2]
  assert_equal "世界", "こんにちわ世界"[-2..-1]
  assert_equal "んに", "こんにちわ世界"[1,2]
  assert_equal "世", "こんにちわ世界"["世"]
  assert_equal "世", "こんにちわ世界"["世"]
end
