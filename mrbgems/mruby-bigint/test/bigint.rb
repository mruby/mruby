assert 'Bigint basic' do
  n = 1<<65
  assert_equal 36893488147419103232, n
end

assert 'Bigint +' do
  n = 1<<65
  assert_equal 36893488147419103232, n + 0
  assert_equal 36893488147419104229, n + 997
  assert_equal 36893488147419102235, n + -997
  assert_equal(-36893488147419102235, -n + 997)
  assert_equal(-36893488147419104229, -n + -997)
  assert_equal 73786976294838206464, n + n
  assert_equal 0, n + -n
  assert_equal 0, -n + n
  assert_equal(-73786976294838206464, -n + -n)
  assert_equal 36893488147419104229, 997 + n
  assert_equal 36893488147419102235, -997 + n
end

assert 'Bigint -' do
  n = 1<<65
  assert_equal 36893488147419103232, n - 0
  assert_equal 36893488147419102235, n - 997
  assert_equal 36893488147419104229, n - -997
  assert_equal(-36893488147419104229, -n - 997)
  assert_equal(-36893488147419102235, -n - -997)
  assert_equal 0, n - n
  assert_equal(-36893488147419104229, -997 - n)
  assert_equal(-36893488147419102235, 997 - n)
  assert_equal(-36893488147419104229, -997 - n)
end

assert 'Bigint *' do
  n = 1<<65
  assert_equal 0, n * 0
  assert_equal 36782807682976845922304, n * 997
  assert_equal(-36782807682976845922304, n * -997)
  assert_equal 36782807682976845922304, 997 * n
  assert_equal(-36782807682976845922304, -997 * n)
  assert_equal 1361129467683753853853498429727072845824, n * n
  assert_equal(-1361129467683753853853498429727072845824, -n * n)
  assert_equal(-1361129467683753853853498429727072845824, n * -n)
  assert_equal 1361129467683753853853498429727072845824, -n * -n

  # Test multiplication commutativity for large numbers with different limb counts
  # This test specifically targets the bug where operands with different
  # limb counts would produce different results based on order
  a = (2**512) - 1      # 16 limbs
  b = 26815615859885194199148049996411692254958731641184786755447122887443528060147093953603748596333806855380063716372972101707507765623893139892867298012168194  # 17 limbs
  assert_equal(a * b, b * a)
end

assert 'Bigint /' do
  n = 1<<65
  assert_equal 37004501652376231, n / 997
  assert_equal(-37004501652376232, n / -997)
  assert_equal(-37004501652376232, -n / 997)
  assert_equal 0, 997 / n
  assert_equal 2, 73786976294838206464 / n
  assert_equal 1, n / n
  assert_equal(-1, -n / n)
  assert_equal(-1, n / -n)
  assert_equal 1, -n / -n
end

assert 'Bigint mod' do
  n = 1<<65
  assert_equal 925, n % 997
  assert_equal(-72, n % -997)
  assert_equal 72, -n % 997
  assert_equal(-925, -n % -997)
  assert_equal 0, n % n
  assert_equal 997, 997 % n
  assert_equal 36893488147419102235, -997 % n
  assert_equal(-36893488147419102235, 997 % -n)
  assert_equal(-997, -997 % -n)
  assert_equal 18446744073709551616, (n / 2) % n
end

assert 'Bigint divmod' do
  n = 1<<65
  assert_equal [37004501652376231, 925], n.divmod(997)
  assert_equal [-37004501652376232, -72], n.divmod(-997)
  assert_equal [-37004501652376232, 72], (-n).divmod(997)
  assert_equal [37004501652376231, -925], (-n).divmod(-997)
  assert_equal [1, 0], n.divmod(n)
  assert_equal [0, 997], 997.divmod(n)
  assert_equal [-1, 36893488147419102235], (-997).divmod(n)
  assert_equal [-1, -36893488147419102235], 997.divmod(-n)
  assert_equal [0, -997], (-997).divmod(-n)
  assert_equal [0, 18446744073709551616], (n / 2).divmod(n)
end

assert 'Bigint &' do
  n = 1<<65
  assert_equal 0, n & 0
  assert_equal 0, 0 & n
  assert_equal 0, n & 1
  assert_equal 1, (n + 3) & 1
  assert_equal 2, (n + 3) & 2
  assert_equal 3, (n + 3) & 3
  assert_equal n, n & n
  assert_equal 36893488147419103232, n & -1
  assert_equal 36893488147419103232, -1 & n
end

assert 'Bigint |' do
  n = 1<<65
  assert_equal 36893488147419103232, n | 0
  assert_equal 36893488147419103232, 0 | n
  assert_equal 36893488147419103233, n | 1
  assert_equal 36893488147419103233, 1 | n
  assert_equal 36893488147419103235, n | 3
  assert_equal 36893488147419103232, n | n
  assert_equal(-1, n | -1)
end

assert 'Bigint ^' do
  n = 1<<65
  assert_equal 36893488147419103232, n ^ 0
  assert_equal 36893488147419103233, n ^ 1
  assert_equal 36893488147419103235, 3 ^ n
  assert_equal 0, n ^ n
  assert_equal(-36893488147419103233, n ^ -1)
  assert_equal(-36893488147419103231, -n ^ 1)
end

assert 'Bigint to_s' do
  n = 1197857166996989179607278372168909873645893814254642585755536286462800958278984531968
  assert_equal n, "11978_571669_96989179607278372168909873645893814254642585755536286462800958278984531968".to_i
  assert_equal(-n, "-11978_571669_96989179607278372168909873645893814254642585755536286462800958278984531968".to_i)
  n = 0x1197857166996989179607278372168909873645893814254642585755536286462800958278984531968
  assert_equal n, "1197857166996989179607278372168909873645893814254642585755536286462800958278984531968".to_i(16)
  n = 10 ** 20
  assert_equal "100000000000000000000", n.to_s
end

assert 'Bigint pow' do
  n = 18446744073709551616
  assert_equal n, 2 ** 64
  assert_equal n, 1 << 64
  assert_equal 2, n >> 63

  n = 1<<65
  assert_equal n, n ** 1
  assert_equal 1, n ** 0
  assert_equal 1361129467683753853853498429727072845824, n ** 2
  # assert_equal 193128586, n.pow(n, 1234567890)
  # assert_equal(-1041439304, n.pow(n, -1234567890))
end

assert 'Bigint abs' do
  n = 1<<65
  assert_equal 36893488147419103232, n.abs
  assert_equal 36893488147419103232, (-n).abs
end
