# A pattern may hold more than 256 character classes: the table grows to what
# the pattern needs. Built at run time and as a literal, each matching as
# CRuby's does.

assert('Regexp holds more than 256 character classes') do
  skip unless __ENCODING__ == "UTF-8"
  alts = (0...400).map { |i| "[#{(0x4E00 + i).chr("UTF-8")}#{(0x3041 + i % 80).chr("UTF-8")}]q" }
  re = Regexp.new(alts.join("|"))
  s = "a#{(0x4E00 + 399).chr("UTF-8")}q b#{(0x4E00 + 7).chr("UTF-8")}q cぁq"
  assert_equal ["侏q", "万q", "ぁq"], s.scan(re)
  assert_nil "nothing" =~ re
end

assert('Regexp literal of many character classes') do
  lit = /[a1][b2][c3][d4][e5][f6][g7][h8][i9][j0]|[k1][l2][m3][n4][o5][p6][q7][r8][s9][t0]/
  assert_equal [], "a1b2c3d4e5f6g7h8i9j0 k1".scan(lit)
  assert_equal ["abcdefghij", "k2m4o6q8s0"], "abcdefghij k2m4o6q8s0".scan(lit)
end
