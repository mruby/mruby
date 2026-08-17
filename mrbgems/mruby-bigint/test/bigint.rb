assert 'Bigint basic' do
  n = 1<<65
  assert_equal 36893488147419103232, n
end

assert 'Bigint ==' do
  n = 1<<64

  # A Bigint receiver used to be read as a plain `mrb_int`, so `==` compared
  # the low bits of the magnitude instead of the number itself.
  assert_false n == 0
  assert_false n + 5 == 5
  assert_false n - 1 == -1
  assert_false(-(n + 5) == 5)
  assert_false (1<<160) + 5 == 5
  assert_false (1<<192) + 5 == 5
  assert_false n.zero?
  assert_false [n].include?(0)

  assert_true n == 1<<64
  assert_false n == 1<<65
  assert_false n == -n
  assert_false 5 == n + 5

  assert_false n == 'x'
  assert_false n == nil

  if Object.const_defined?(:Float)
    assert_true (1<<70) == (2.0**70)
    assert_false (1<<70) == (2.0**71)

    # A Float argument used to be compared by rounding the big integer to a
    # Float, so every neighbour within half an ULP answered equal.
    assert_false (1<<70) + 1 == (2.0**70)
    assert_false (1<<70) - 1 == (2.0**70)
    assert_false(-((1<<70) + 1) == (-2.0**70))
    assert_true(-(1<<70) == (-2.0**70))

    # Infinity and NaN reach the same comparison and have no integer part.
    assert_false (1<<70) == (1.0/0.0)
    assert_false (1<<70) == (-1.0/0.0)
    assert_false (1<<70) == (0.0/0.0)

    # A Float whose integer part an `mrb_int` holds, zero included, is carried
    # over as one rather than built into a big integer.
    assert_false (1<<70) == 0.0
    assert_false (1<<70) == -0.0
    assert_false (1<<70) == 0.5
    assert_false (1<<70) == 1.5
    assert_false(-(1<<70) == -0.5)

    # ...and every one of them written the other way round. `Float#==` had no
    # arm for a big integer, so it fell through to false and the two
    # directions disagreed about an equal pair.
    assert_true (2.0**70) == (1<<70)
    assert_false (2.0**71) == (1<<70)
    assert_true(-2.0**70 == -(1<<70))
    assert_false (2.0**70) == (1<<70) + 1
    assert_false (2.0**70) == (1<<70) - 1
    assert_false(-2.0**70 == -((1<<70) + 1))
    assert_false (1.0/0.0) == (1<<70)
    assert_false (0.0/0.0) == (1<<70)
    assert_false 0.5 == (1<<70)
  end
end

assert 'Bigint eql?' do
  n = 1<<70

  assert_true n.eql?(1<<70)
  assert_false n.eql?(1<<71)
  assert_false n.eql?(0)
  assert_false n.eql?(-n)

  assert_false n.eql?('x')
  assert_false n.eql?(nil)

  # `eql?` compares class as well as value, so an equal Float is not eql? to a
  # big integer even though `==` holds between the two.
  if Object.const_defined?(:Float)
    assert_false n.eql?(2.0**70)
    assert_true n == (2.0**70)
    assert_false (2.0**70).eql?(n)
    assert_true (2.0**70) == n
  end
end

assert 'Bigint <=>' do
  n = 1<<70

  assert_equal 0, n <=> (1<<70)
  assert_equal(-1, n <=> (1<<71))
  assert_equal 1, n <=> 0
  assert_equal(-1, 0 <=> n)

  assert_nil n <=> 'x'
  assert_nil n <=> nil

  # `cmpnum()` used to round the big integer to a Float before comparing it
  # against one, which collapsed a whole neighbourhood onto the same answer.
  if Object.const_defined?(:Float)
    assert_equal 0, n <=> (2.0**70)
    assert_equal 1, n + 1 <=> (2.0**70)
    assert_equal(-1, n - 1 <=> (2.0**70))
    assert_equal 0, (2.0**70) <=> n
    assert_equal(-1, (2.0**70) <=> n + 1)
    assert_equal 1, (2.0**70) <=> n - 1

    assert_true n + 1 > (2.0**70)
    assert_true (2.0**70) < n + 1
    assert_true n - 1 < (2.0**70)
    assert_true (2.0**70) > n - 1

    assert_equal(-1, n <=> (1.0/0.0))
    assert_equal 1, n <=> (-1.0/0.0)
    assert_nil n <=> (0.0/0.0)
    assert_nil (0.0/0.0) <=> n

    # A Float with no integer part of its own is compared the same way.
    assert_equal 1, n <=> 0.0
    assert_equal 1, n <=> 0.5
    assert_equal 1, n <=> -0.5
    assert_equal(-1, -n <=> 0.5)
    assert_equal(-1, 0.5 <=> n)
    assert_equal 1, 0.5 <=> -n
  end
end

assert 'Bigint normalizes the smallest Integer' do
  # An Integer's negative range is one wider than its positive one: where
  # mrb_int is 64 bits the smallest Integer is -(2**63), and it cannot stay a
  # big integer, since the same value built out of fixnums alone has to be
  # indistinguishable from it. The exponent that is not this build's is asked
  # too, where both spellings already share one representation, two fixnums on
  # a 64 bit build and two big integers on a 32 bit one, and the rows hold for
  # that reason.
  [31, 63].each do |e|
    from_bigint = -(2 ** e)
    from_fixnum = -(2 ** e - 1) - 1

    assert_equal from_fixnum, from_bigint
    assert_true from_bigint.eql?(from_fixnum)
    assert_true from_fixnum.eql?(from_bigint)
    assert_equal from_fixnum.hash, from_bigint.hash
  end
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

  # `**` compares by value and passes either way; the result must also be
  # demoted to a fixnum Integer, or `eql?` and `hash` disagree with it.
  one = n ** 0
  assert_true one.eql?(1)
  assert_true 1.eql?(one)
  assert_equal 1.hash, one.hash
  h = {}
  200.times { |i| h[i] = i }
  assert_equal 1, h[one]
  assert_equal 1361129467683753853853498429727072845824, n ** 2
  # assert_equal 193128586, n.pow(n, 1234567890)
  # assert_equal(-1041439304, n.pow(n, -1234567890))
end

assert 'Bigint Integer#pow(e, m) - Montgomery path' do
  # Regression: mpz_powm_montgomery() failed to pre-reduce base mod n,
  # producing wrong results when base >= n. Also trim() must restore
  # the canonical sn=0 when sz becomes 0, otherwise an inconsistent
  # zero bignum (sn!=0, sz=0) propagates through the squaring loop.
  m = (2**40) + 1
  assert_equal 1, (2**160).pow(2, m)
  assert_equal 1, (2**320).pow(2, m)
  assert_equal 8, ((2**160) + 1).pow(3, m)
  m2 = (2**100) + 3
  assert_equal (3**500) % m2, (3**500).pow(1, m2)
  assert_equal ((5**300) ** 7) % m2, (5**300).pow(7, m2)
end

assert 'Bigint Integer#remainder large operand' do
  # Regression: mpz_mod's Barrett path didn't enforce its precondition
  # x < 2^(2*bits(m)), so it silently truncated high limbs when x was
  # much larger than m^2, producing the wrong remainder. Integer#%
  # took the udiv path and worked, but Integer#remainder went through
  # mpz_mod and was broken.
  m = (2**100) + 3
  assert_equal (3**500) % m, (3**500).remainder(m)
  assert_equal (5**500) % ((2**150) + 1), (5**500).remainder((2**150) + 1)
  assert_equal (2**400) % ((2**130) + 1), (2**400).remainder((2**130) + 1)
end

assert 'Bigint abs' do
  n = 1<<65
  assert_equal 36893488147419103232, n.abs
  assert_equal 36893488147419103232, (-n).abs
end

assert 'Bigint gcd' do
  # zero cases
  assert_equal 0, 0.gcd(0)
  n = 1 << 200
  assert_equal n, n.gcd(0)
  assert_equal n, 0.gcd(n)

  # power-of-2 fast path
  assert_equal 1 << 100, (1 << 200).gcd(1 << 100)
  assert_equal 1 << 100, (1 << 100).gcd(1 << 200)
  assert_equal 1 << 40, (10 ** 50).gcd(1 << 40)

  # negative operands: result is the positive GCD
  a = 1 << 200
  b = 3 << 200
  assert_equal a, a.gcd(b)
  assert_equal a, (-a).gcd(b)
  assert_equal a, a.gcd(-b)
  assert_equal a, (-a).gcd(-b)

  # balanced multi-limb with known common factor
  fib1000 = (1..1000).inject([0, 1]) { |(x, y), _| [y, x + y] }[0]
  common = fib1000
  k, m = 1_000_003, 1_000_033          # small coprime primes
  assert_equal common, (common * k).gcd(common * m)
  assert_equal common, (common * m).gcd(common * k)

  # unbalanced: small coprime vs large
  big = common * k
  assert_equal 1, big.gcd(m)
  assert_equal 1, m.gcd(big)

  # Fibonacci neighbors are always coprime
  f100 = (1..100).inject([0, 1]) { |(x, y), _| [y, x + y] }[0]
  f101 = (1..101).inject([0, 1]) { |(x, y), _| [y, x + y] }[0]
  assert_equal 1, f100.gcd(f101)

  # Euclidean fallback path: operand sizes differ by several limbs
  assert_equal 7, (7 * (1 << 4000)).gcd(7 * 13)
end

assert 'Bigint large integer literal' do
  # A literal too large for mrc_int exercises the bignum path of the code
  # generator (issue #6905). Compare against the runtime-computed value.
  assert_equal 10**40, 10000000000000000000000000000000000000000
  assert_equal(-(10**40), -10000000000000000000000000000000000000000)
  assert_equal 2**128, 340282366920938463463374607431768211456
end

assert 'OP_LOADL does not retain a boxed Integer in the GC arena' do
  # `OP_LOADL` boxes the pool entry afresh on every execution rather than
  # handing back a stored object: the pool holds an integer as a raw i32 or i64
  # and a big integer as its digits, and none of the three is an `mrb_value`.
  # Boxing allocates for a value outside the fixnum range, and the opcode has
  # no cfunc epilogue behind it to shrink the arena, so a loop over such a
  # literal retains one object per iteration unless the opcode restores.
  #
  # This belongs with the arena assertions in test/t/gc.rb and lives here
  # because both literals are wider than 32 bits: without mruby-bigint they are
  # out of mrb_int range on an MRB_INT32 build, and an unrepresentable literal
  # is a compile-time error rather than something a test can rescue.  With the
  # gem they reach IREP_TT_INT64 where mrb_int is 64 bits wide and
  # IREP_TT_BIGINT where it is not, which are the two branches that allocate.
  #
  # `GC.stat` is a cfunc and so reports the count before its own epilogue
  # restores.  The loop body avoids sends because a single one would empty the
  # arena and hide the retention.
  GC.start
  base = GC.stat[:live]
  i = 0
  while i < 20000
    z = 2147483648           # outside the fixnum range under NaN boxing
    z = 4611686018427387904  # outside it under 64-bit word boxing
    i += 1
  end
  assert_operator GC.stat[:live] - base, :<, 5000
  assert_equal 4611686018427387904, z
end

assert('Integer - a Float too big for an mrb_int keeps every digit') do
  skip unless Object.const_defined?(:Float)
  # The conversion took the highest base-2**DIG_SIZE digit and read zero for
  # every digit below it, because the fraction left behind was never scaled
  # back up: 1e20 answered 92233720368547758080, which is 5 * 2**64.
  assert_equal 100000000000000000000, 1e20.to_i
  assert_equal(-100000000000000000000, -1e20.to_i)
  assert_equal 10000000000000000000, 1e19.to_i
  assert_equal 150000000000000000000, 1.5e20.to_i
  assert_equal 1000000000000000019884624838656, 1e30.to_i
  assert_equal 30000000000000000570425344, 3.0e25.to_i
  # A power of two answered correctly all along, its lower digits being zero.
  assert_equal 18446744073709551616, (2.0**64).to_i
  assert_equal 1180591620717411303424, (2.0**70).to_i
  # Every one of them names the Float it came from.
  [1e19, 1e20, 1.5e20, 1e30, 3.0e25, 1e100, 2.0**70].each do |f|
    assert_equal f, f.to_i.to_f
  end
end

assert('Bigint an mrb_int operand keeps every bit') do
  # An mrb_int was written into a fixed pair of limbs, which holds the whole
  # value only while a limb is half an mrb_int wide.  Under MRB_NO_MPZ64BIT a
  # limb is 16 bits, so every bit from 32 up was dropped on the way in.  This
  # is the entry every fixnum overflow takes.
  max = (1 << 62) - 1 + (1 << 62)  # MRB_INT_MAX where an mrb_int is 64 bits
  assert_equal 9223372036854775808, max + 1
  assert_equal 9223372036854775808, (1 << 62) * 2
  assert_equal(-9223372036854775809, -max - 2)
  assert_equal(-13835058055282163712, -(1 << 62) * 3)

  # ...and the same conversion on the mrb_int operand of a big integer.  Every
  # expected value is named outright, because writing one as an expression over
  # (1 << 32) would take the same broken path and agree with the wrong answer.
  assert_equal 18446744078004518913, (1 << 64) + ((1 << 32) + 1)
  assert_equal 18446744069414584319, (1 << 64) - ((1 << 32) + 1)
  assert_equal 79228162514264337593543950336, (1 << 64) * (1 << 32)
end
