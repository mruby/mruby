assert('Carriage return in here documents', '8.7.6.3.6') do
  a = <<AAA
aaa
AAA
  b = <<BBB
bbb
BBB
  assert_equal "aaa\n", a
  assert_equal "bbb\n", b
end
